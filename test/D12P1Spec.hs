module D12P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D12
import D12P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D12P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            25 @=? distance [Forward 10,GoNorth 3,Forward 7,TurnRight 90,Forward 11]
    ]