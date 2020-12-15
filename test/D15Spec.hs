module D15Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D15

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D15 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            436 @=? spokennumber 2020 [0,3,6]
    ]