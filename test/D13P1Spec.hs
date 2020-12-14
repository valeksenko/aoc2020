module D13P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D13P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D13P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            295 @=? busid 939 [7,13,59,31,19]
    ]