module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Main (toNumbers)

main :: Effect Unit
main = runTest suites

suites :: TestSuite
suites = do
  suite "input" do
    test "reads numbers"
      $ Assert.equal (Just [ 1, 2 ]) (toNumbers "1\n2\n")
