module D12P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D12
import D12P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D12P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            286 @=? distance [Forward 10,GoNorth 3,Forward 7,TurnRight 90,Forward 11]
    ]