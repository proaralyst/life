module Main where

import Life (loop, init, newKeyChannel, runKeyChannel)

import System.IO (hSetBuffering, stdin, BufferMode(..))
import Control.Concurrent (forkIO)
import Control.Monad (void)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  channel <- newKeyChannel
  forkIO $ runKeyChannel channel
  void $ loop Life.init channel
