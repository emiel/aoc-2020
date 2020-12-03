module Main where

import Prelude
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Array (length, filter)
import Data.String (split, Pattern(..), indexOf, splitAt, drop, trim)
import Data.String.CodeUnits (charAt, singleton, toCharArray)
import Effect (Effect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

data Policy
  = Policy Int Int Char

derive instance eqPolicy :: Eq Policy

instance showPolicy :: Show Policy where
  show (Policy min max char) = "Policy " <> show min <> " " <> show max <> " " <> show char

lines :: String -> Array String
lines = split (Pattern "\n")

splitChar :: String -> Char -> Maybe { before :: String, after :: String }
splitChar str c = case res of
  Nothing -> Nothing
  Just r -> Just { before: r.before, after: drop 1 r.after }
  where
  res = case indexOf (Pattern $ singleton c) str of
    Just idx -> Just (splitAt idx str)
    Nothing -> Nothing

-- 9-14 r
parsePolicy :: String -> Maybe Policy
parsePolicy buf = case (splitChar buf ' ') of
  Nothing -> Nothing
  Just parts -> case (splitChar parts.before '-') of
    Nothing -> Nothing
    Just minMax -> case (fromString minMax.before) of
      Nothing -> Nothing
      Just min -> case (fromString minMax.after) of
        Nothing -> Nothing
        Just max -> case (charAt 0 parts.after) of
          Nothing -> Nothing
          Just char -> Just (Policy min max char)

-- Part one
passesPolicy :: Policy -> String -> Boolean
passesPolicy (Policy min max chr) password = charCount >= min && charCount <= max
  where
  charCount = length $ filter (\c -> c == chr) $ toCharArray password

-- Part two
passesTobogganPolicy :: Policy -> String -> Boolean
passesTobogganPolicy (Policy min max chr) password = case charAt (min - 1) password of
  Nothing -> false
  Just minChr -> case charAt (max - 1) password of
    Nothing -> false
    Just maxChr -> (minChr == chr || maxChr == chr) && minChr /= maxChr

-- 9-14 r: rrrrrrkrrrcrrrr
validLine :: (Policy -> String -> Boolean) -> String -> Boolean
validLine f line = case (splitChar line ':') of
  Nothing -> false
  Just parts -> case (parsePolicy parts.before) of
    Nothing -> false
    Just policy -> f policy (trim parts.after)

process :: (Policy -> String -> Boolean) -> Array String -> Array Boolean
process f xs = (validLine f)<$> xs

report :: Array Boolean -> Int
report results = length $ filter (\b -> b == true) results

main :: Effect Unit
main = do
  buf <- readTextFile UTF8 "./input.txt"
  logShow $ report $ (process passesPolicy) $ lines buf
  logShow $ report $ (process passesTobogganPolicy) $ lines buf
