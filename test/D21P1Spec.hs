module D21P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D21
import D21P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D21P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            5 @=? nonallergens [(["mxmxvkd","kfcds","sqjhc","nhms"],["dairy","fish"]),(["trh","fvjkl","sbzzf","mxmxvkd"],["dairy"]),(["sqjhc","fvjkl"],["soy"]),(["sqjhc","mxmxvkd","sbzzf"],["fish"])]
    ]