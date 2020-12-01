module D01P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D01P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D01P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            Just 514579 @=? findentries [1721, 979, 366, 299, 675, 1456]
    ]