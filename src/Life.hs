{-# LANGUAGE NamedFieldPuns, DeriveGeneric, DeriveAnyClass #-}
module Life
    ( loop
    , init
    ) where

import Prelude hiding (init)

import GHC.Generics
import qualified Data.HashMap.Strict as HM
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

data Coord =
    Coord
    { x :: Int
    , y :: Int
    } deriving (Show, Eq, Ord, Generic, Hashable)

data Model =
    Model
    { generation :: Int
    , life :: HM.HashMap Coord Bool
    } deriving (Show)

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
init = Model 0 life
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
neighboursOf Coord{x, y} =
  [ Coord (x' `mod` maxX) (y' `mod` maxY)
  | x' <- (+x) <$> [-1..1]
  , y' <- (+y) <$> [-1..1]
  , x /= x' || y /= y'
  ]


updateCell :: Bool -> Int -> Bool
updateCell True x
    | x < 2 = False
    | x < 4 = True
    | otherwise = False
updateCell False 3 = True
updateCell False _ = False

tick :: Model -> Model
tick Model{generation, life} = Model {generation = generation + 1, life = life'}
  where
    life' = (`HM.mapWithKey` life) (\ key value ->
        let neighbours = (`HM.lookup` life) <$> neighboursOf key in
        let toCount x = if x then 1 else 0 in
        let alive = foldl' (+) 0 (toCount . fromMaybe False <$> neighbours) in
        updateCell value alive
        )

view :: Model -> String
view Model{generation, life} =
    board ++ printf "Generation %i\n" generation
  where
    display True = '#'
    display False = ' '
    board = do
        y <- [0..maxY - 1]
        (display . fromMaybe False . (`HM.lookup` life) . (`Coord` y)
            <$> [0..maxX - 1])
            ++ "\n"

loop :: Model -> IO Model
loop model = do
        putStr (view model)
        threadDelay 100
        loop (tick model)
