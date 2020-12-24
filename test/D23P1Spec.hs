module D23P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D23P1
import qualified Data.Map as M

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D23P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            67384529 @=? playcrabcups 100 [3,8,9,1,2,5,4,6,7]
    ]