module D25P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D25P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D25P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            14897079 @=? encryptionkey 5764801 17807724
    ]