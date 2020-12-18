module D17P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D17P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D17P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            848 @=? countactive 6 (unlines [".#.", "..#", "###", ""])
    ]