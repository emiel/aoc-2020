module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Main (splitChar, parsePolicy, Policy(..), passesTobogganPolicy)

main :: Effect Unit
main = runTest suites

suites :: TestSuite
suites = do
  suite "splitChar" do
    test "success" $ Assert.equal (Just { before: "a", after: "b" }) (splitChar "a b" ' ')
  suite "parse policy" do
    test "parses success"
      $ Assert.equal (Just (Policy 1 2 'a')) (parsePolicy "1-2 a")
  suite "toboggan policy" do
    test "1"
      $ Assert.equal true (passesTobogganPolicy (Policy 1 3 'a') "abcde")
    test "2"
      $ Assert.equal false (passesTobogganPolicy (Policy 1 3 'b') "cdefg")
    test "3"
      $ Assert.equal false (passesTobogganPolicy (Policy 2 9 'c') "ccccccccc")
