module D21P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D21
import D21P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D21P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            "mxmxvkd,sqjhc,fvjkl" @=? allergens [(["mxmxvkd","kfcds","sqjhc","nhms"],["dairy","fish"]),(["trh","fvjkl","sbzzf","mxmxvkd"],["dairy"]),(["sqjhc","fvjkl"],["soy"]),(["sqjhc","mxmxvkd","sbzzf"],["fish"])]
    ]