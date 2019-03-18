module Tape where
import Data.Sequence (Seq, update, index, fromList, (<|), (|>))
import Data.Foldable (toList)
import Model (Symbol, Move (LeftShift, RightShift, NoShift))

type HeadPosition = Int
type Tape = (Seq Symbol, HeadPosition)

(<||) :: Symbol -> Tape -> Tape -- append symbol to left
(<||) sym (syms, i) = (sym <| syms, succ i)
(||>) :: Tape -> Symbol -> Tape -- append symbol to right
(||>) (syms, i) sym = (syms |> sym,      i)

stringify :: Tape -> String
stringify (syms, i) = concat (map show (toList syms))

tapeLength :: Tape -> Int
tapeLength (syms, _) = length syms

makeTape :: [Symbol] -> HeadPosition -> Tape
makeTape syms i
  | i >= length syms = error "Not enough tape for head position"
  | otherwise = (fromList syms, i)

readSymbol :: Tape -> Symbol
readSymbol (syms, i) = syms `index` i

writeSymbol :: Symbol -> Tape -> Tape
writeSymbol sym (syms, i) = (update i sym syms, i)

moveHead :: Move -> Tape -> Symbol -> Tape
moveHead NoShift tape _ = tape
moveHead LeftShift tape@(_,i) blankSymbol
  | i == 0 = moveHeadLeft (blankSymbol <|| tape)
  | otherwise = moveHeadLeft tape
moveHead RightShift tape@(_,i) blankSymbol
  | i == (tapeLength tape) - 1 = moveHeadRight (tape ||> blankSymbol)
  | otherwise = moveHeadRight tape

moveHeadLeft :: Tape -> Tape
moveHeadLeft (syms, i) = (syms, i-1)
moveHeadRight :: Tape -> Tape
moveHeadRight (syms, i) = (syms, i+1)
