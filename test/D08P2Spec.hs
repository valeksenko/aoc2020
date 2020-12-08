module D08P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import Bootcode
import D08P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D08P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            8 @=? execmodified [NOP 0,ACC 1,JMP 4,ACC 3,JMP (-3),ACC (-99),ACC 1,JMP (-4),ACC 6]
    ]