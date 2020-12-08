module D08P2 (
    execmodified
) where

import Data.List
import Bootcode

data Modified =
    Modified {
        mPosition :: Position
      , mRun :: [Int]
      , mState :: State
    } deriving(Show, Eq)

execmodified :: [InstructionSet] -> Accumulator
execmodified code = sAccumulator . mState . until halts execUpdated $ Modified (-1) [] (mkBootcode code)
    where
        halts m = (sPosition $ mState m) >= (length code)
        execUpdated m = until secondTimeOrHalt execO $ newModification (1 + mPosition m)
        newModification p = Modified p [] (mkBootcode $ modifyCode p code)
        secondTime m = (sPosition $ mState m) `elem` (mRun m)
        secondTimeOrHalt m =  secondTime m || halts m
        execO m = m { mRun = (sPosition $ mState m):(mRun m), mState = exec (mState m) }

modifyCode :: Int -> [InstructionSet] -> [InstructionSet]
modifyCode p code = take p code ++ [modify $ code !! p] ++ drop (p + 1) code
    where
        modify (JMP a) = NOP a
        modify (NOP a) = JMP a
        modify op = op 

{-
https://adventofcode.com/2020/day/8#part2

After some careful analysis, you believe that exactly one instruction is corrupted.

Somewhere in the program, either a jmp is supposed to be a nop, or a nop is supposed to be a jmp. (No acc instructions were harmed in the corruption of this boot code.)

The program is supposed to terminate by attempting to execute an instruction immediately after the last instruction in the file. By changing exactly one jmp or nop, you can repair the boot code and make it terminate correctly.

For example, consider the same program from above:

nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
If you change the first instruction from nop +0 to jmp +0, it would create a single-instruction infinite loop, never leaving that instruction. If you change almost any of the jmp instructions, the program will still eventually find another jmp instruction and loop forever.

However, if you change the second-to-last instruction (from jmp -4 to nop -4), the program terminates! The instructions are visited in this order:

nop +0  | 1
acc +1  | 2
jmp +4  | 3
acc +3  |
jmp -3  |
acc -99 |
acc +1  | 4
nop -4  | 5
acc +6  | 6
After the last instruction (acc +6), the program terminates by attempting to run the instruction below the last instruction in the file. With this change, after the program terminates, the accumulator contains the value 8 (acc +1, acc +1, acc +6).

Fix the program so that it terminates normally by changing exactly one jmp (to nop) or nop (to jmp). What is the value of the accumulator after the program terminates?
-}