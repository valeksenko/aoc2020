{-# LANGUAGE OverloadedStrings #-}

module D14P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D14
import D14P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D14P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            165 @=? sumval [Mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",Mem (8,11),Mem (7,101),Mem (8,0)]
    ]