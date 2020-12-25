module D24P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D24
import D24P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D24P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            10 @=? fliptiles [[SouthWest,West,SouthEast,West,NorthEast,NorthEast,West,SouthWest,SouthWest,West,SouthWest,East,SouthEast,West,NorthEast,NorthEast,NorthEast,NorthWest,SouthEast,SouthEast],[SouthWest,East,SouthWest,SouthEast,NorthWest,NorthEast,West,SouthEast,West,NorthWest,West,NorthEast,NorthEast,SouthWest,West,NorthWest,NorthWest,SouthEast,NorthEast,East,East,NorthEast],[SouthEast,NorthWest,West,NorthWest,SouthEast,SouthWest,SouthWest,NorthEast,SouthWest,SouthEast],[NorthEast,East,SouthEast,NorthEast,SouthWest,SouthEast,West,NorthEast,West,SouthWest,NorthEast,West,NorthEast,NorthEast,SouthWest,SouthWest,East,SouthEast,NorthEast,NorthWest,NorthWest],[NorthWest,East,SouthEast,NorthEast,NorthEast,NorthWest,West,SouthEast,NorthWest,NorthEast,SouthWest,NorthEast,SouthWest,East,West,SouthWest],[NorthEast,SouthEast,NorthWest,West,West,SouthEast,NorthWest,NorthWest,SouthWest,NorthWest,NorthEast,SouthWest,SouthWest,SouthEast,NorthWest,SouthEast,East,East],[SouthEast,West,West,NorthWest,NorthEast,West,SouthEast,NorthWest,SouthEast,NorthEast,NorthEast,NorthEast,NorthEast,West,SouthEast],[East,West,NorthWest,West,West,SouthWest,East,West,East,East,SouthEast,East,West,West,NorthWest,East,West],[NorthEast,NorthWest,SouthEast,SouthEast,SouthEast,West,NorthWest,SouthEast,West,NorthEast,SouthEast,NorthWest,West,NorthWest,West,NorthEast,NorthEast,SouthEast,East,East,SouthWest,West],[NorthWest,SouthWest,SouthWest,NorthWest,SouthWest,West,NorthWest,East,SouthEast,SouthWest,East,NorthEast],[West,NorthEast,SouthEast,SouthEast,NorthEast,NorthWest,SouthEast,NorthWest,SouthEast,West,NorthEast,NorthEast,NorthEast,SouthWest,West,SouthEast,West,SouthWest,NorthWest,NorthEast],[SouthWest,NorthWest,SouthEast,NorthWest,NorthEast,SouthWest,East,NorthWest,East,SouthWest,NorthWest,West,SouthEast,SouthWest,NorthEast,West,East,NorthWest,West,NorthEast,East],[SouthEast,NorthEast,SouthWest,SouthWest,NorthEast,West,West,NorthEast,East,West,NorthEast,NorthWest,East,NorthEast,NorthEast,SouthWest,NorthEast,SouthWest,NorthEast,East,SouthWest],[NorthEast,West,SouthWest,SouthEast,NorthEast,West,NorthWest,SouthWest,NorthEast,NorthWest,East,West,SouthEast,NorthEast,SouthEast,East,West,SouthWest],[SouthEast,SouthEast,NorthWest,East,SouthEast,SouthWest,West,NorthEast,NorthWest,SouthEast,SouthWest,NorthEast,NorthEast,SouthWest,West,SouthWest,NorthWest,SouthEast,NorthEast,East],[West,SouthEast,SouthEast,SouthEast,West,SouthEast,East,West,SouthEast,NorthWest,NorthEast,West,NorthWest,NorthEast,SouthEast,NorthEast,SouthEast,NorthEast,NorthWest,West],[SouthWest,East,West,SouthEast,NorthEast,NorthWest,SouthEast,NorthEast,NorthEast,SouthWest,West,East,NorthWest,SouthWest,West,NorthEast,NorthEast],[SouthEast,NorthEast,SouthEast,West,East,NorthEast,SouthWest,East,SouthEast,West,NorthWest,NorthWest,NorthWest,NorthEast,SouthEast,NorthWest,SouthWest,NorthWest,SouthWest,NorthEast,East],[East,West,East,East,SouthWest,NorthWest,SouthEast,West,SouthEast,East,SouthEast,NorthWest,East,SouthEast,NorthWest,NorthWest,West,East,NorthWest,SouthWest,NorthEast],[West,NorthEast,SouthWest,West,West,NorthWest,SouthEast,NorthEast,NorthWest,East,East,East,West,SouthEast,West]]
    ]