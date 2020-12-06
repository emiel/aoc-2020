module Main where

import Data.Array (length, index, cons, catMaybes, filter)
import Data.Maybe (Maybe(..))
import Data.String (split, Pattern(..), trim, joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse, maximum)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Prelude

lines :: String -> Array String
lines = split (Pattern "\n")

unlines :: Array String -> String
unlines = joinWith "\n"

data Square
  = Empty
  | Tree

instance showSquare :: Show Square where
  show Empty = "."
  show Tree = "#"

derive instance eqSquare :: Eq Square

data Coord
  = Coord Int Int

type Row a
  = Array a

data Forest
  = Forest (Array (Row Square))

derive instance eqForest :: Eq Forest

instance showForest :: Show Forest where
  show (Forest xss) = joinWith "" $ showLine <$> xss
    where
    showLine xs = (joinWith "" $ show <$> xs) <> "\n"

parseSquare :: Char -> Maybe Square
parseSquare c = case c of
  '.' -> Just Empty
  '#' -> Just Tree
  _ -> Nothing

parseLine :: String -> Maybe (Row Square)
parseLine line =
  let
    chars = toCharArray $ trim line
  in
    traverse parseSquare chars

parseForest :: String -> Maybe Forest
parseForest input = case (traverse parseLine (lines (trim input))) of
  Nothing -> Nothing
  Just xss -> Just $ Forest xss

forestHeight :: Forest -> Int
forestHeight (Forest xss) = length xss

forestWidth :: Forest -> Int
forestWidth (Forest xss) = case (maximum $ length <$> xss) of
  Just max -> max
  Nothing -> 0

squareAt :: Coord -> Forest -> Maybe Square
squareAt (Coord x y) forest@(Forest rows) = do
  let
    width = forestWidth (forest)
  row <- index rows y
  square <- index row (x `mod` width)
  pure square

makeSlope :: Int -> Int -> (Coord -> Coord)
makeSlope deltaX deltaY = \(Coord x y) -> Coord (x + deltaX) (y + deltaY)

walk :: Int -> (Coord -> Coord) -> Forest -> Array (Maybe Square)
walk = go (Coord 0 0)
  where
  go coord@(Coord _ y) height slope forest
    | y > height = []
    | otherwise = cons (squareAt coord forest) (go (slope coord) height slope forest)

solveWithSlope :: (Coord -> Coord) -> Forest -> Int
solveWithSlope slope forest = length $ filter ((==) Tree) $ catMaybes (walk height slope forest)
  where
  height = forestHeight forest

solveA :: Forest -> Int
solveA forest = solveWithSlope (makeSlope 3 1) forest

-- solveB :: Forest -> Int
-- solveB forest = product $ map (($) forest) <$> slopes
--   where
--   slopes = [ (makeSlope 1 1), (makeSlope 3 1), (makeSlope 5 1), (makeSlope 7 1), (makeSlope 1 2) ]
solveB :: Forest -> Int
solveB forest = a * b * c * d * e
  where
  a = solveWithSlope (makeSlope 1 1) forest

  b = solveWithSlope (makeSlope 3 1) forest

  c = solveWithSlope (makeSlope 5 1) forest

  d = solveWithSlope (makeSlope 7 1) forest

  e = solveWithSlope (makeSlope 1 2) forest

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./input.txt"
  case parseForest input of
    Nothing -> log "Couldn't parse input"
    Just forest -> do
      -- logShow $ forest
      log "part a"
      logShow $ solveA forest
      log "part b"
      logShow $ solveB forest
