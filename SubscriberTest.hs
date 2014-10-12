{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.ZMQ4.Monadic
import qualified Data.ByteString as BS
import Control.Monad.IO.Class
import System.IO

main :: IO ()
main = runZMQ $ do
  subscriber <- socket Sub
  connect subscriber "tcp://localhost:5556"
  subscribe subscriber BS.empty

  loop subscriber
  where loop subscriber = do
          update <- receive subscriber
          liftIO $ BS.hPutStrLn stdout update
          loop subscriber

