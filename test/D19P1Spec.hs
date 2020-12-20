module D19P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D19
import D19P1
import qualified Data.Map as M

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D19P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            let rules = M.fromList [(0,[RULE [4,1,5]]),(1,[RULE [2,3],RULE [3,2]]),(2,[RULE [4,4],RULE [5,5]]),(3,[RULE [4,5],RULE [5,4]]),(4,[LITERAL 'a']),(5,[LITERAL 'b'])]
            True  @=? matchMessage rules "ababbb"
            True  @=? matchMessage rules "abbbab"
            False @=? matchMessage rules "bababa"
            False @=? matchMessage rules "aaabbb"
            False @=? matchMessage rules "aaaabbb"
    ]