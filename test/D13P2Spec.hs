module D13P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D13P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D13P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            1068781 @=? simpletimestamp [(0,7),(1,13),(4,59),(6,31),(7,19)]
    ]