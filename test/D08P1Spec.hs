module D08P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import Bootcode
import D08P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D08P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            5 @=? execonce [NOP 0,ACC 1,JMP 4,ACC 3,JMP (-3),ACC (-99),ACC 1,JMP (-4),ACC 6]
    ]