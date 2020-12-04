module D04 (
    mkDocuments
  , requiredFields
  , Field
  , Document
) where

import Data.List
import Data.Tuple.Extra

type Field = (String, String)
type Document = [Field]

requiredFields = [
        "byr"
      , "iyr"
      , "eyr"
      , "hgt"
      , "hcl"
      , "ecl"
      , "pid"
      -- , "cid"
    ]

mkDocuments :: String -> [Document]
mkDocuments = filter (not . null) . foldl' parse [[]] . lines
    where
        parse (x:xs) l = if null l then []:x:xs else (x ++ (fields $ words l)):xs 
        fields = map splitF
        splitF = second tail . break ((==) ':')
