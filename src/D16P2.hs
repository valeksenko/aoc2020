module D16P2 (
    idfields
) where

import Data.List
import Data.Tuple.Extra

type Field = Int
type Rule = (Field, Field)
type FieldRule = (String, [Rule])
type RuleSet = [FieldRule]
type Ticket = [Field]

idfields :: Ticket -> RuleSet -> [Ticket] -> Int
idfields myticket rules = product . filterDeparture . sortRules rules . filter validTicket
    where
        validTicket = all (\f -> validField allRules f)
        allRules = concatMap snd rules
        filterDeparture = map fst . filter departure . zip myticket
        departure (_, (d, _)) = "departure " `isPrefixOf` d

sortRules :: RuleSet -> [Ticket] -> RuleSet
sortRules rules = map fst . sortOn snd . ruleToField . matchRules . zip [0..] . transpose
    where
        matchRules fields = map (match fields) rules
        match fields rule = (rule, foldr (matching $ snd rule) [] fields)
        matching r (n, f) l = if all (validField r) f then n:l else l

validField :: [Rule] -> Field -> Bool
validField r f = any (inRange f) r
    where
        inRange f (l, u) = (f >= l) && (f <= u)

ruleToField :: [(FieldRule, [Int])] -> [(FieldRule, Int)]
ruleToField l = map r2f . fst $ until (null . snd) extractFound ([], l)
    where
        r2f (r, f:_) = (r, f)
        extractFound (found, rest) = first ((++) found) . partition ((==) 1 . length . snd) $ map (unique $ concatMap snd found) rest
        unique l (r, l') = (r, l' \\ l)


{-
https://adventofcode.com/2020/day/16#part2

Now that you've identified which tickets contain invalid values, discard those tickets entirely. Use the remaining valid tickets to determine which field is which.

Using the valid ranges for each field, determine what order the fields appear on the tickets. The order is consistent between all tickets: if seat is the third field, it is the third field on every ticket, including your ticket.

For example, suppose you have the following notes:

class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
Based on the nearby tickets in the above example, the first position must be row, the second position must be class, and the third position must be seat; you can conclude that in your ticket, class is 12, row is 11, and seat is 13.

Once you work out which field is which, look for the six fields on your ticket that start with the word departure. What do you get if you multiply those six values together?
-}