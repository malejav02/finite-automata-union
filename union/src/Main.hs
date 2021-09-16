module Main where

import Union

import Numeric.Natural (Natural)
import Data.Set ( Set )                 --Test
import qualified Data.Set as Set        --Test      

import Language.Mira.FA.Types           --Test

----------------------------------------------
-- Automaton accepting strings ending in 01
fa1 :: FA Natural
fa1 = MkFA (Set.fromList [1,2,3])
           (Set.fromList [Move 1 '1' 1, Move 1 '0' 2, Move 2 '1' 3, Move 2 '0' 2, Move 3 '1' 1, Move 3 '0' 2])
           (1)
           (Set.fromList [3]) 


-- Automaton accepting strings begining in 10
fa2 :: FA Natural
fa2 = MkFA (Set.fromList [4,5,6])
           (Set.fromList[Move 4 '1' 5, Move 5 '0' 6, Move 6 '1' 6, Move 6 '0' 6])
           (4)
           (Set.fromList [6])

main :: IO()
main = print $ union fa1 fa2
