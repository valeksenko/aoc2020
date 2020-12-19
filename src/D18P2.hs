module D18P2 (
    exec
) where

import D18
import Data.List

type Stack = ([Int], [Statement], [Statement])

opPriorities = [ADD, MUL]

exec :: [Statement] -> Int
exec =  res . execStack . mkStack
    where
        res (n, _, o) = applyStatement n o

execStack :: Stack -> Stack
execStack st@(_, [], _) = st
execStack (n, ADD:l, o) = execStack (n, l, (ADD:o)) 
execStack (n, MUL:l, o) = execStack (n, l, (MUL:o))  
execStack (n, (NUM i):l, o) = execStack (i:n, l, o)
execStack (n, START:l, o) = applyStack n o . execStack $ mkStack l
execStack (n, END:l, o) = ([applyStatement n o], l, [])

execOp :: Int -> Int -> Statement -> Int
execOp n i ADD = n + i 
execOp n i MUL = n * i 

mkStack :: [Statement] -> Stack
mkStack s = ([0], s, [ADD])

applyStack :: [Int] -> [Statement] -> Stack -> Stack
applyStack n o (i, l, _) = execStack (i ++ n, l, o) 

applyStatement :: [Int] -> [Statement] -> Int
applyStatement n o = head . fst $ foldl' applyOp (n, o) opPriorities

applyOp :: ([Int], [Statement]) -> Statement -> ([Int], [Statement])
applyOp (lastN:restN, ops) op = foldl' apply ([lastN], []) $ zip ops restN
    where
        apply (l:n, s) (o, i) = if o == op then ((execOp l i o):n, s) else (i:l:n, o:s)

{-
https://adventofcode.com/2020/day/18#part2

You manage to answer the child's questions and they finish part 1 of their homework, but get stuck when they reach the next section: advanced math.

Now, addition and multiplication have different precedence levels, but they're not the ones you're familiar with. Instead, addition is evaluated before multiplication.

For example, the steps to evaluate the expression 1 + 2 * 3 + 4 * 5 + 6 are now as follows:

1 + 2 * 3 + 4 * 5 + 6
  3   * 3 + 4 * 5 + 6
  3   *   7   * 5 + 6
  3   *   7   *  11
     21       *  11
         231
Here are the other examples from above:

1 + (2 * 3) + (4 * (5 + 6)) still becomes 51.
2 * 3 + (4 * 5) becomes 46.
5 + (8 * 3 + 9 + 3 * 4 * 3) becomes 1445.
5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) becomes 669060.
((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 becomes 23340.
What do you get if you add up the results of evaluating the homework problems using these new rules?
-}