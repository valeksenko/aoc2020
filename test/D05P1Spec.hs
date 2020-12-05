module D05P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D05P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D05P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            f <- readFile "data/d05.txt"
            976 @=? maxseat (lines f)
    ]