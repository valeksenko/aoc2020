module D01P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D01P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D01P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            Just 241861950 @=? findtripleentries [1721, 979, 366, 299, 675, 1456]
    ]