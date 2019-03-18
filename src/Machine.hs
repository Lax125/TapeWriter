module Machine where
import Data.Sequence (index)
import Data.Set (member, insert)
import Model
import Tape

type Machine = (Model, Tape, State)

validTape :: Tape -> Model -> Bool
validTape (syms, _) (_,_,bsym,insyms,_,_,_) =
  all ((flip member) validsyms) syms where
    validsyms = insert bsym insyms

feed :: Tape -> Model -> Machine
feed tape model@(_,_,_,_,s0,_,_)
  | not (validTape tape model) = error "Invalid tape"
  | otherwise = (model, tape, s0)

extract :: Machine -> Tape
extract (_,tape,_) = tape

run :: Machine -> Machine
run machine
  | isHalted machine = machine
  | otherwise = run (step machine)

step :: Machine -> Machine
step machine@(model@(_,_,bsym,_,_,_,tf), tape, s)
  | isHalted machine = machine
  | otherwise = (model, moveHead move (writeSymbol writesym tape) bsym, nexts) where
      (nexts, writesym, move) = tf (s, readSymbol tape)

stepList :: Machine -> [Machine]
stepList machine
  | isHalted machine = [machine]
  | otherwise = machine : stepList (step machine)

isHalted :: Machine -> Bool
isHalted ((_,_,_,_,_,accss,_),_,s) = s `member` accss