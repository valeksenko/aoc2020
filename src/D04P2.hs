module D04P2 (
    validdocs
) where

import Data.List
import D04

validdocs :: [Document] -> Int
validdocs = length . filter null . map ((\\) requiredFields . map fst . filter validField)
    where
        validField (k, v) = case k of
            "byr" -> (all (flip elem ['0'..'9']) v) && (length v == 4) && ((read v::Int) >= 1920) && ((read v::Int) <= 2002)
            "iyr" -> (all (flip elem ['0'..'9']) v) && (length v == 4) && ((read v::Int) >= 2010) && ((read v::Int) <= 2020)
            "eyr" -> (all (flip elem ['0'..'9']) v) && (length v == 4) && ((read v::Int) >= 2020) && ((read v::Int) <= 2030)
            "hgt" -> isHeight v
            "hcl" -> (length v == 7) && (head v == '#') && (all (flip elem (['0'..'9'] ++ ['a'..'f'])) $ tail v)
            "ecl" -> elem v ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
            "pid" -> (length v == 9) && (all (flip elem ['0'..'9']) v)
            otherwise -> True 


isHeight :: String -> Bool
isHeight v = valid $ take (length v - 2) v
    where
        valid d
            | ("cm" `isSuffixOf` v) = ((read d::Int) >= 150) && ((read d::Int) <= 193)
            | ("in" `isSuffixOf` v) = ((read d::Int) >= 59) && ((read d::Int) <= 76)
            | otherwise = False

{-
https://adventofcode.com/2020/day/4#day2

You can continue to ignore the cid field, but each other field has strict rules about what values are valid for automatic validation:

byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.

Count the number of valid passports - those that have all required fields and valid values. Continue to treat cid as optional. In your batch file, how many passports are valid?
-}