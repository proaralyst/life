{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}
module Life
    ( loop
    , init
    , KeyChannel(..)
    , newKeyChannel
    , runKeyChannel
    ) where

import Prelude hiding (init)

import GHC.Generics
import qualified Data.HashMap.Strict as HM
import Control.Concurrent (threadDelay, forkIO, yield)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryReadMVar)
import Control.Monad (forever)
import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import System.Console.ANSI (clearScreen)
import Text.Printf (printf)
import Control.Lens

data Coord =
    Coord
    { _x :: !Int
    , _y :: !Int
    } deriving (Show, Eq, Ord, Generic, Hashable)

makeLenses ''Coord

data Model =
    Model
    { _generation :: !Int
    , _life :: HM.HashMap Coord Bool
    , _lastInput :: Char
    } deriving (Show)

makeLenses ''Model

maxY = 25
maxX = 80


{-
  01234567
0 xxx
1 x
2  x   x
3      x
4      x
-}

init :: Model
init = Model 0 life 'x'
  where
    dead = HM.fromList $ do
        x <- [0..maxX - 1]
        y <- [0..maxY - 1]
        return (Coord x y, False)
    coords =  uncurry Coord <$>
        [ (0, 0)
        , (0, 1)
        , (1, 0)
        , (1, 2)
        , (2, 0)
        , (5, 2)
        , (5, 3)
        , (5, 4)
        ]
    life =
        foldl' (\ acc coord -> HM.insert coord True acc) dead coords

neighboursOf :: Coord -> [Coord]
neighboursOf coord =
  [ Coord (x' `mod` maxX) (y' `mod` maxY)
  | x' <- (+coord^.x) <$> [-1..1]
  , y' <- (+coord^.y) <$> [-1..1]
  , coord^.x /= x' || coord^.y /= y'
  ]


updateCell :: Bool -> Int -> Bool
updateCell True x
    | x < 2 = False
    | x < 4 = True
    | otherwise = False
updateCell False 3 = True
updateCell False _ = False

tick :: Maybe Char -> Model -> Model
tick input model =
    Model (model^.generation + 1) life' (fromMaybe (model^.lastInput) input)
  where
    life' = (`HM.mapWithKey` (model^.life)) (\ key value ->
        let neighbours = (`HM.lookup` (model^.life)) <$> neighboursOf key in
        let toCount x = if x then 1 else 0 in
        let alive = foldl' (+) 0 (toCount . fromMaybe False <$> neighbours) in
        updateCell value alive
        )

render :: Model -> String
render model =
    board ++ printf "Generation %i\tLast input %c\n" (model^.generation) (model^.lastInput)
  where
    display True = '#'
    display False = ' '
    board = do
        y <- [0..maxY - 1]
        (display . fromMaybe False . (`HM.lookup` (model^.life)) . (`Coord` y)
            <$> [0..maxX - 1])
            ++ "\n"


runRender :: Model -> IO ()
runRender model = clearScreen >> putStr (render model)

newtype KeyChannel = KeyChannel {unKeyChannel :: MVar (Char, KeyChannel)}

newKeyChannel :: IO KeyChannel
newKeyChannel = KeyChannel <$> newEmptyMVar

runKeyChannel :: KeyChannel -> IO ()
runKeyChannel channel = do
  char <- getChar
  channel' <- newKeyChannel
  forkIO $ putMVar (unKeyChannel channel) (char, channel')
  runKeyChannel channel'

tryReadKeyChannel :: KeyChannel -> IO (Maybe (Char, KeyChannel))
tryReadKeyChannel channel = tryReadMVar (unKeyChannel channel)

loop :: Model -> KeyChannel -> IO Model
loop model keyChannel = do
  runRender model
  threadDelay (1000 * 100)
  message <- tryReadKeyChannel keyChannel
  let (input, keyChannel') =
        (fst <$> message, maybe keyChannel snd message)
  loop (tick input model) keyChannel'
