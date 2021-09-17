-- Implementation of FA's union using functional programming in Haskell.

-- Maria Alejandra Vélez Clavijo y Alejandra Palacio Jaramillo.

-- Windows 10.
-- Tested with GHC 9.0.1 and QuickCheck 2.14.2

module Union
    ( union
    ) where
 
import Language.Mira.FA.Types
import Numeric.Natural (Natural)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (size)
import qualified Data.Set as Set 
--
 
-- |The union function takes two finite automata and returns one finite automata, which is the union of the other two.
union :: FA Natural -> FA Natural -> FA Natural
union (MkFA statesFA1 transFA1 initialstateFA1 acceptingstatesFA1 )
      (MkFA statesFA2 transFA2 initialstateFA2 acceptingstatesFA2 )
        =   MkFA (Set.fromList (newstates (Set.toList statesFA1) (Set.toList statesFA2 )))
                 (Set.fromList (f (Set.toList statesFA1) (Set.toList transFA1) initialstateFA1 (Set.toList statesFA2) (Set.toList transFA2) initialstateFA2))
                 (0)
                 (Set.fromList (newacceptingstate (statesmap (Set.toList statesFA1) 1) (Set.toList acceptingstatesFA1)++newacceptingstate (statesmap (Set.toList statesFA2) (fromIntegral (length statesFA1)+1)) (Set.toList acceptingstatesFA2))) 
                     
-- |This function concatenates the accepting states of the two automatas that receives.
acceptingstates :: [Natural] -> [Natural] -> [Natural]
acceptingstates list1 list2 = list1 ++ list2

-- |This function receives a map, a list of accepting states and returns a list with the
-- associated values between the map and the list.
newacceptingstate:: (Map Natural Natural) -> [Natural]->[Natural]
newacceptingstate map []=[]
newacceptingstate   map (x:xs) = [map Map.! x]++ (newacceptingstate map xs)
            
-- |This function receives the states of each automata and replace them with a list since 
-- 1 until the sum of the number of states of the two automata (the zero represents the new
-- initial state).
newstates:: [Natural]-> [Natural]->[Natural]
newstates statesFA1 statesFA2 = [0]++[1 .. (fromIntegral (length statesFA1 + length statesFA2))]

-- |This function receives the transitions of the two automata and concatenates them is a list. 
movementslist:: [Move Natural]-> [Move Natural]-> [Move Natural]
movementslist movementsFA1 movementsFA2=  movementsFA1 ++ movementsFA2

-- |This function receives a list with the union of the states of the two automata 
-- and a number that represents an iterator, where the keys are going to be each 
-- states and the values are going to be the iterator.
statesmap:: [Natural]-> Natural -> Map Natural Natural
statesmap [] cont = Map.empty 
statesmap (x:xs) cont = Map.insert x cont (statesmap xs (cont+1))

-- |This function receives a list with the transitions of the automata and a map that contains 
-- the states of the union automata. The function implements recursively an other function 
-- (movements2) to modify each movement with the states of the union automata. 
movements1:: [Move Natural] -> Map Natural Natural -> [Move Natural]
movements1 [] map =[]
movements1 (x:xs) map=  [movements2 x map ] ++ (movements1 xs map )

-- |This function receives a movement, a map and returns a movement. The function updates the states
-- of each movement with the new states of the union automata. 
movements2 :: Move Natural -> Map Natural Natural -> Move Natural
movements2 (Move state1 char state2) map = Move (map Map.! state1) char (map Map.! state2)
movements2 (Emove state1 state2) map = Emove (map Map.! state1)  (map Map.! state2)

-- |The f function receives the states, transitions and the initial state of each automata and returns 
-- a list with the transitions (including the epsilon transitions since the new initial state created 
-- until each initial state of the two automata) of the union automata.
f :: [Natural] -> [Move Natural] -> Natural -> [Natural] -> [Move Natural] -> Natural -> [Move Natural]
f states1 moves1 num1 states2 moves2 num2 = (movements1 moves1 (statesmap states1 1))++(movements1 moves2 (statesmap states2 ((fromIntegral (length states1)+1))))++[Emove 0 (((statesmap states1 1) Map.! num1)), Emove 0 (((statesmap states2 ((fromIntegral (length states1)+1))) Map.! num2))]








    
