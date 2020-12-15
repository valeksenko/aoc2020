{-# LANGUAGE OverloadedStrings #-}

module D14P2 (
    sumval
) where

import D14
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Bits

type Storage = (BitMask, M.Map Address Register)

sumval :: [InstructionSet] -> Integer
sumval = sum . M.elems . snd . foldl' execOp mkStorage
    where
        mkStorage = ("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", M.empty)

execOp :: Storage -> InstructionSet -> Storage
execOp (_, s) (Mask mask) = (mask, s)

execOp storage@(mask, _) (Mem (addr, register)) = foldr setRegister storage addresses
    where
        setRegister a (m, s) = (m, M.insert a register s)
        addresses = map convert . foldr apply [[]] . zip [0..] . reverse $ T.unpack mask
        add l = concatMap (\e -> map (e:) l)
        convert = foldr toI 0 . zip [0..]
        toI (i, r) = (.|.) (r `shift` i)
        apply (i, e) l = case e of
            '0' -> add l [if testBit addr i then 1 else 0]
            '1' -> add l [1]
            'X' -> add l [0,1]

{-
https://adventofcode.com/2020/day/14#part2

For some reason, the sea port's computer system still can't communicate with your ferry's docking program. It must be using version 2 of the decoder chip!

A version 2 decoder chip doesn't modify the values being written at all. Instead, it acts as a memory address decoder. Immediately before a value is written to memory, each bit in the bitmask modifies the corresponding bit of the destination memory address in the following way:

If the bitmask bit is 0, the corresponding memory address bit is unchanged.
If the bitmask bit is 1, the corresponding memory address bit is overwritten with 1.
If the bitmask bit is X, the corresponding memory address bit is floating.
A floating bit is not connected to anything and instead fluctuates unpredictably. In practice, this means the floating bits will take on all possible values, potentially causing many memory addresses to be written all at once!

For example, consider the following program:

mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
When this program goes to write to memory address 42, it first applies the bitmask:

address: 000000000000000000000000000000101010  (decimal 42)
mask:    000000000000000000000000000000X1001X
result:  000000000000000000000000000000X1101X
After applying the mask, four bits are overwritten, three of which are different, and two of which are floating. Floating bits take on every possible combination of values; with two floating bits, four actual memory addresses are written:

000000000000000000000000000000011010  (decimal 26)
000000000000000000000000000000011011  (decimal 27)
000000000000000000000000000000111010  (decimal 58)
000000000000000000000000000000111011  (decimal 59)
Next, the program is about to write to memory address 26 with a different bitmask:

address: 000000000000000000000000000000011010  (decimal 26)
mask:    00000000000000000000000000000000X0XX
result:  00000000000000000000000000000001X0XX
This results in an address with three floating bits, causing writes to eight memory addresses:

000000000000000000000000000000010000  (decimal 16)
000000000000000000000000000000010001  (decimal 17)
000000000000000000000000000000010010  (decimal 18)
000000000000000000000000000000010011  (decimal 19)
000000000000000000000000000000011000  (decimal 24)
000000000000000000000000000000011001  (decimal 25)
000000000000000000000000000000011010  (decimal 26)
000000000000000000000000000000011011  (decimal 27)
The entire 36-bit address space still begins initialized to the value 0 at every address, and you still need the sum of all values left in memory at the end of the program. In this example, the sum is 208.

Execute the initialization program using an emulator for a version 2 decoder chip. What is the sum of all values left in memory after it completes?
-}