module D06P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D06P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D06P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            11 @=? groupquestions ["abc","","a","b","c","","ab","ac","","a","a","a","a","","b"]
    ]