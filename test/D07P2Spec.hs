module D07P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D07P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D07P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            126 @=? bagamount ("shiny","gold") [(("shiny","gold"),[(("dark","red"),2)]),(("dark","red"),[(("dark","orange"),2)]),(("dark","orange"),[(("dark","yellow"),2)]),(("dark","yellow"),[(("dark","green"),2)]),(("dark","green"),[(("dark","blue"),2)]),(("dark","blue"),[(("dark","violet"),2)]),(("dark","violet"),[])]
    ]