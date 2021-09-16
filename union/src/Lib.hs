-- Representation of FA's union using functional programming in Haskell.

-- Maria Alejandra Vélez Clavijo y Alejandra Palacio Jaramillo.

-- Windows 10.
-- Tested with GHC 9.0.1 and QuickCheck 2.14.2

module Lib
    ( union
    ) where
 
import Language.Mira.FA.Types
import Numeric.Natural (Natural)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (size)
import qualified Data.Set as Set 
--
 
union :: FA Natural -> FA Natural -> FA Natural
union (MkFA statesFA1 transFA1 initialstateFA1 acceptingstatesFA1 )
      (MkFA statesFA2 transFA2 initialstateFA2 acceptingstatesFA2 )
        =   MkFA (Set.fromList (newstates (Set.toList statesFA1) (Set.toList statesFA2 )))
                 (Set.fromList (f (Set.toList statesFA1) (Set.toList transFA1) initialstateFA1 (Set.toList statesFA2) (Set.toList transFA2) initialstateFA2))
                 (0)
                 (Set.fromList (newacceptingstate (statesmap (Set.toList statesFA1) 1) (Set.toList acceptingstatesFA1)++newacceptingstate (statesmap (Set.toList statesFA2) (fromIntegral (length statesFA2)+1)) (Set.toList acceptingstatesFA2))) 
                     
acceptingstates :: [Natural] -> [Natural] -> [Natural]
acceptingstates list1 list2 = list1 ++ list2

newacceptingstate:: (Map Natural Natural) -> [Natural]->[Natural]
newacceptingstate map []=[]
newacceptingstate   map (x:xs) = [map Map.! x]++ (newacceptingstate map xs)
            
newstates:: [Natural]-> [Natural]->[Natural]
newstates estadosFA1 estadosFA2 = [0 .. (fromIntegral (length estadosFA1 + length estadosFA2))]

movementslist:: [Move Natural]-> [Move Natural]-> [Move Natural]
movementslist movementsFA1 movementsFA2=  movementsFA1 ++ movementsFA2

statesmap:: [Natural]-> Natural -> Map Natural Natural
statesmap [] cont = Map.empty 
statesmap (x:xs) cont = Map.insert x cont (statesmap xs (cont+1))

movements1:: [Move Natural] -> Map Natural Natural -> [Move Natural]
movements1 [] map =[]
movements1 (x:xs) map=  [movements2 x map ] ++ (movements1 xs map )

movements2 :: Move Natural -> Map Natural Natural -> Move Natural
movements2 (Move state1 char state2) map = Move (map Map.! state1) char (map Map.! state2)
movements2 (Emove state1  state2) map = Emove (map Map.! state1)  (map Map.! state2)

f :: [Natural] -> [Move Natural] -> Natural -> [Natural] -> [Move Natural] -> Natural -> [Move Natural]
f states1 moves1 num1 states2 moves2 num2 = (movements1 moves1 (statesmap states1 1))++(movements1 moves2 (statesmap states2 ((fromIntegral (length states1)+1))))++[Emove 0 (((statesmap states1 1) Map.! num1)), Emove 0 (((statesmap states2 ((fromIntegral (length states1)+1))) Map.! num2))]








    
