module D17P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D17P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D17P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            112 @=? countactive 6 (unlines [".#.", "..#", "###", ""])
    ]