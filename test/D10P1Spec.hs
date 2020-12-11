module D10P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D10P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D10P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            35 @=? jolts [16,10,15,5,1,11,7,19,6,12,4]
            220 @=? jolts [28,33,18,42,31,14,46,20,48,47,24,23,49,45,19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3]
    ]