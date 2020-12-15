{-# LANGUAGE OverloadedStrings #-}

module D14P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D14
import D14P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D14P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            208 @=? sumval [Mask "000000000000000000000000000000X1001X",Mem (42,100),Mask "00000000000000000000000000000000X0XX",Mem (26,1)]
    ]