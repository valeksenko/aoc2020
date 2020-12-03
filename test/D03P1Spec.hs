module D03P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D03
import D03P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D03P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            7 @=? counttrees (Toboggan (0,0) 11 11 [(10,10),(8,10),(4,10),(1,10),(10,9),(5,9),(4,9),(0,9),(7,8),(3,8),(2,8),(0,8),(10,7),(1,7),(10,6),(5,6),(3,6),(1,6),(5,5),(4,5),(2,5),(9,4),(6,4),(5,4),(1,4),(10,3),(8,3),(4,3),(2,3),(9,2),(6,2),(1,2),(8,1),(4,1),(0,1),(3,0),(2,0)] 0)
    ]