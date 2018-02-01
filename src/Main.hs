module Main where

import Prelude hiding (interact)

import Life (Model(..), Interact(Tick), init, render, interact, keymap)

import Control.Concurrent (threadDelay, forkIO, ThreadId)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, tryReadTQueue, writeTQueue)
import Control.Monad (void)
import Control.Monad.STM (atomically)
import System.Console.ANSI (clearScreen)
import System.IO (hSetBuffering, stdin, BufferMode(..))

forkKeyWorker :: TQueue Char -> IO ThreadId
forkKeyWorker queue = forkIO loop
  where
    loop = do
      char <- getChar
      atomically $ writeTQueue queue char
      loop

loop :: TQueue Char -> Model -> IO ()
loop keys model = do
  clearScreen
  putStr $ render model
  threadDelay (1000 * 100)
  interaction <- (>>= keymap model) <$> atomically (tryReadTQueue keys)
  loop keys (interact . Tick $ maybe model interact interaction)


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  keyQueue <- atomically newTQueue
  forkKeyWorker keyQueue
  void $ loop keyQueue Life.init
