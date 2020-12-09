module D09P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D09P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D09P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            62 @=? weakness 5 [35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]
    ]