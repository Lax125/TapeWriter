module Main where
import Data.Set (fromList)
import Model
import Tape
import Machine

-- 4-state busy beaver
myModel :: Model
myModel = program myTF (binaryModel (fromList [0..5]) 1 (fromList [0]))

myTF :: TransitionFunction
myTF (1, Zero) = (2, One, RightShift)
myTF (1, One)  = (2, One, LeftShift)
myTF (2, Zero) = (1, One, LeftShift)
myTF (2, One)  = (3, Zero, LeftShift)
myTF (3, Zero) = (0, One, RightShift)
myTF (3, One)  = (4, One, LeftShift)
myTF (4, Zero) = (4, One, RightShift)
myTF (4, One)  = (1, Zero, RightShift)

myTape :: Tape
myTape = makeTape (take 18 (repeat Zero)) 12

myMachine :: Machine
myMachine = feed myTape myModel

main :: IO ()
main = mapM_ putStrLn [stringify (extract m) | m <- (stepList myMachine)]
