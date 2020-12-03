module Main where

import Prelude
import Control.MonadZero (guard)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.Traversable (traverse, sum)
import Effect (Effect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

toNumbers :: String -> Maybe (Array Int)
toNumbers buf = traverse fromString (lines $ trim buf)
  where
  lines = split (Pattern "\n")

-- how to take a slice of the array (i need array indexes...)
pairs :: Array Int -> Array (Array Int)
pairs xs = do
  i <- xs
  j <- xs -- XXX(emiel) start at index of i
  guard $ sum [ i, j ] == 2020
  pure [ i, j ]

triples :: Array Int -> Array (Array Int)
triples xs = do
  i <- xs
  j <- xs
  k <- xs
  guard $ sum [ i, j, k ] == 2020
  pure [ i, j, k ]

findPairs :: String -> Array (Array Int)
findPairs xs = case toNumbers xs of
  (Just ys) -> pairs ys
  Nothing -> [ [] ]

findTriples :: String -> Array (Array Int)
findTriples xs = case toNumbers xs of
  (Just ys) -> triples ys
  Nothing -> [ [] ]

main :: Effect Unit
main = do
  str <- readTextFile UTF8 "./input.txt"
  logShow $ findPairs str
  logShow $ findTriples str
