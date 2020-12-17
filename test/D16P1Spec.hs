module D16P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D16P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D16P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            71 @=? errorrate [[(1,3),(5,7)],[(6,11),(33,44)],[(13,40),(45,50)]] [
                        [7,3,47]
                      , [40,4,50]
                      , [55,2,20]
                      , [38,6,12]
                    ]
    ]