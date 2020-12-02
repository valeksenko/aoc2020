module D02P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D02P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D02P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            1 @=? validpasswords [
                    (((1, 3), 'a'), "abcde")
                  , (((1, 3), 'b'), "cdefg")
                  , (((2, 9), 'c'), "ccccccccc")
                ]
    ]