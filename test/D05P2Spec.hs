module D05P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D05P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D05P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            f <- readFile "data/d05.txt"
            Just 685 @=? findseat (lines f)
    ]