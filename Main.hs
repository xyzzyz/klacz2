{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable #-}
module Main where

import Control.Exception (finally, catch, Exception(..))
import Control.Exception.Base
import Control.Concurrent
import Control.Concurrent.STM

import Data.Typeable
import qualified Data.ByteString as BS
import qualified Data.Map as M

import System.IO
import System.IO.Unsafe

import Network.Socket
import Network (connectTo, withSocketsDo, PortID(..))
import Network.IRC

bsCrLf = BS.pack [13, 10]
hPutBSCrLf handle bs = BS.hPut handle bs >> BS.hPut handle bsCrLf

botNickname = "klacz2Adam"
botUser = "klacz"
botRealName = "Klacz"
botIrcHost = "irc.freenode.net"
botChannels = ["#klacztest"]

data IRCException = IRCException String
                  deriving (Show, Typeable)

instance Exception IRCException

childDiesTMVar = unsafePerformIO newEmptyTMVarIO

zmqWriterChan :: TChan Message
zmqWriterChan = unsafePerformIO newTChanIO

ircWriterChan :: TChan Message
ircWriterChan = unsafePerformIO newTChanIO


loginToIrcNetwork sock = do
  let nickMessage = nick botNickname
      userMessage = user botUser "8" "*" botRealName
  hPutBSCrLf sock (encode nickMessage)
  hPutBSCrLf sock (encode userMessage)
  -- we should check here whether login was successful,
  -- but let's ignore it for now

handlePingMessage pingMsg = do
  let pingArgument = head . msg_params $ pingMsg
  BS.putStrLn $ "PING " `BS.append` pingArgument
  atomically $ writeTChan ircWriterChan (pong pingArgument)

handleIrcMessage msg = do
  case msg_command msg of
    "PING" -> handlePingMessage msg
    _ -> atomically $ writeTChan zmqWriterChan msg

handleSocketLine line = do
  let lineNoCr = BS.take (BS.length line - 1) line
  case decode lineNoCr of
    Just msg -> handleIrcMessage msg
    Nothing -> throw (IRCException $ "couldn't parse irc line: "
                      ++ show (BS.unpack line))

ircReader sock = do
  loginToIrcNetwork sock
  ircReaderLoop sock

ircReaderLoop sock = do
  line <- BS.hGetLine sock
  if BS.null line
    then putStrLn "Socket: Got empty line, wut"
    else handleSocketLine line >> ircReaderLoop sock

ircWriterLoop sock = do
  msg <- atomically $ readTChan ircWriterChan
  BS.hPutStr sock (encode msg)
  ircWriterLoop sock

zmqWriterLoop = do
  msg <- atomically $ readTChan zmqWriterChan
  putStrLn $ show msg
  zmqWriterLoop

zmqReaderLoop = do
  threadDelay $ 5*1000*1000
  zmqReaderLoop

start sock = do
  forkChild "irc reader" $ ircReader sock
  forkChild "zmq writer" $ zmqWriterLoop
  forkChild "zmq reader" $ zmqReaderLoop
  forkChild "irc writer" $ ircWriterLoop sock
  return ()
  where forkChild :: String -> IO a -> IO ThreadId
        forkChild name m = forkFinally m (notifyParent name)
        notifyParent :: String -> Either SomeException a -> IO ()
        notifyParent name s =
          let msg = case s of
                Left e -> show e
                Right _ -> "Success" in
          atomically $ putTMVar childDiesTMVar (name ++ ": " ++ msg)


main :: IO ()
main = withSocketsDo $ do
  sock <- connectTo botIrcHost $ PortNumber 6667
  threadDelay $ 5*1000*1000
  hSetBuffering sock LineBuffering
  start sock
  s <- atomically $ takeTMVar childDiesTMVar
  putStrLn $ "Child died: " ++ s
  return ()
