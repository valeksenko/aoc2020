module D18P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D18
import D18P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D18P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            46     @=? (sum . map (exec . parseStatement) $ lines "2 * 3 + (4 * 5)")
            1445   @=? (sum . map (exec . parseStatement) $ lines "5 + (8 * 3 + 9 + 3 * 4 * 3)")
            669060 @=? (sum . map (exec . parseStatement) $ lines "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
            23340  @=? (sum . map (exec . parseStatement) $ lines "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
    ]