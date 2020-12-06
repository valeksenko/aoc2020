module D06P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D06P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D06P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            6 @=? groupquestions ["abc","","a","b","c","","ab","ac","","a","a","a","a","","b"]
    ]