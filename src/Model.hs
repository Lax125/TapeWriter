module Model where
import Data.Set (Set, fromList, union, insert)

type Model = ( Set State          -- states
             , Set Symbol         -- symbols
             , Symbol             -- blank symbol
             , Set Symbol         -- input symbols
             , State              -- initial state
             , Set State          -- accepting states
             , TransitionFunction -- transition function
             )
type State = Integer
type TransitionFunction = (State, Symbol) -> (State, Symbol, Move)

data Symbol = Blank | Zero | One deriving (Eq, Ord)
instance Show Symbol where
  show Blank = "_"
  show Zero  = "0"
  show One   = "1"
data Move = LeftShift | RightShift | NoShift deriving (Eq, Show)

-- make non-programmed binary model with symbols {Zero, One}
binaryModel :: Set State -> State -> Set State -> Model
binaryModel states initialState haltingStates =
  ( insert initialState (haltingStates `union` states)
  , fromList [Zero, One]
  , Zero
  , fromList [One]
  , initialState
  , haltingStates
  , noOpTransition
  )

ternaryModel :: Set State -> State -> Set State -> Model
ternaryModel states initialState haltingStates =
  ( insert initialState (haltingStates `union` states)
  , fromList [Blank, Zero, One]
  , Blank
  , fromList [Zero, One]
  , initialState
  , haltingStates
  , noOpTransition
  )

noOpTransition :: (State, Symbol) -> (State, Symbol, Move)
noOpTransition (state, symbol) = (state, symbol, NoShift)

program :: TransitionFunction -> Model -> Model
program tf (ss,syms,bsym,insyms,s0,accss,_) =
           (ss,syms,bsym,insyms,s0,accss,tf)