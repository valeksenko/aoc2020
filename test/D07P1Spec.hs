module D07P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D07P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D07P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            4 @=? bagamount ("shiny","gold") [(("light","red"),[(("bright","white"),1),(("muted","yellow"),2)]),(("dark","orange"),[(("bright","white"),3),(("muted","yellow"),4)]),(("bright","white"),[(("shiny","gold"),1)]),(("muted","yellow"),[(("shiny","gold"),2),(("faded","blue"),9)]),(("shiny","gold"),[(("dark","olive"),1),(("vibrant","plum"),2)]),(("dark","olive"),[(("faded","blue"),3),(("dotted","black"),4)]),(("vibrant","plum"),[(("faded","blue"),5),(("dotted","black"),6)]),(("faded","blue"),[])]
    ]