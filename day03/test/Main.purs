module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Main

main :: Effect Unit
main = runTest suites

someForest :: Forest
someForest =
  Forest
    [ [ Empty, Tree, Tree, Tree ]
    , [ Tree, Empty, Tree, Tree ]
    , [ Tree, Tree, Empty, Tree ]
    ]

suites :: TestSuite
suites = do
  suite "lines" do
    test "lines0"
      $ Assert.equal [ "#" ] (lines "#")
    test "lines1 -- silly lines!!!"
      $ Assert.equal [ "#", "" ] (lines "#\n")
  suite "forest" do
    test "showForest0"
      $ Assert.equal "#\n" (show $ Forest [ [ Tree ] ])
    test "showForest1"
      $ Assert.equal "#\n#\n" (show $ Forest [ [ Tree ], [ Tree ] ])
    test "parseLine0"
      $ Assert.equal (Just [ Tree, Empty, Tree ]) (parseLine "#.#")
    test "parseLine1"
      $ Assert.equal (Just [ Tree, Empty, Tree ]) (parseLine "#.#\n")
    test "parseForest0"
      $ Assert.equal (Just (Forest [ [ Tree ] ])) (parseForest "#\n")
    test "parseForest1"
      $ Assert.equal
          (Just (Forest [ [ Tree, Tree, Tree ], [ Empty, Empty, Empty ], [ Tree, Empty, Tree ] ]))
          (parseForest "###\n...\n#.#\n")
    test "forestWidth"
      $ Assert.equal 4 (forestWidth someForest)
    test "forestHeight"
      $ Assert.equal 3 (forestHeight someForest)
    test "squareAt0"
      $ Assert.equal (Just Empty) (squareAt (Coord 0 0) someForest)
    test "squareAt1"
      $ Assert.equal (Just Empty) (squareAt (Coord 1 1) someForest)
    test "squareAt2"
      $ Assert.equal (Just Empty) (squareAt (Coord 2 2) someForest)
    test "squareAt3"
      $ Assert.equal (Just Tree) (squareAt (Coord 3 0) someForest)
    test "squareAt4"
      $ Assert.equal (Just Tree) (squareAt (Coord 3 2) someForest)
    test "square out of bounds"
      $ Assert.equal Nothing (squareAt (Coord 99 99) someForest)
    test "squareAtWider0"
      $ Assert.equal (Just Empty) (squareAt (Coord 4 0) someForest)
    test "squareAtWider1"
      $ Assert.equal (Just Empty) (squareAt (Coord 5 1) someForest)
    test "squareAtWider2"
      $ Assert.equal (Just Empty) (squareAt (Coord 6 2) someForest)
    -- test "walk1"
    --   $ Assert.equal ([ Just Empty, Just Tree ]) (walk 3 (makeSlope 3 1) someForest)
    test "solve1"
      $ Assert.equal 1 (solveA someForest)
