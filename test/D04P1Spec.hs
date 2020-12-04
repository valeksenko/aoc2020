module D04P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D04P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D04P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            2 @=? validdocs [[("hcl","#cfa07d"),("eyr","2025"),("pid","166559648"),("iyr","2011"),("ecl","brn"),("hgt","59in")],[("hcl","#ae17e1"),("iyr","2013"),("eyr","2024"),("ecl","brn"),("pid","760753108"),("byr","1931"),("hgt","179cm")],[("iyr","2013"),("ecl","amb"),("cid","350"),("eyr","2023"),("pid","028048884"),("hcl","#cfa07d"),("byr","1929")],[("ecl","gry"),("pid","860033327"),("eyr","2020"),("hcl","#fffffd"),("byr","1937"),("iyr","2017"),("cid","147"),("hgt","183cm")]]
    ]