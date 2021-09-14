module Lib  where
 
import Language.Mira.FA.Types
import Numeric.Natural (Natural)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (size)
import qualified Data.Set as Set 

 
union :: FA Natural -> FA Natural -> FA Natural
union (MkFA estadosFA1 transicionesFA1 estadoInicialFA1 estadosAceptacionFA1 )
      (MkFA estadosFA2 transicionesFA2 estadoInicialFA2 estadosAceptacionFA2 )
        =   (MkFA (nuevoestados  (estadosFA1 estadosFA2 ))
            (movimientosconj ( 
                listamovimientos  (transicionesFA1 transicionesFA2)
                mapaestados ( nuevoestados  estadosFA1 estadosFA2) 0 ))
            (0)
            ( estadosacept estadosAceptacionFA1 estadosAceptacionFA2) 
estadosacept :: [Natural]->[Natural]-> [Natural]
estadosacept list1 list2 = list1 ++ list2
            
nuevoestados:: [Natural]-> [Natural]->[Natural]
nuevoestados estadosFA1 estadosFA2 = [0 .. (fromIntegral (length estadosFA1 + length estadosFA2))]

listamovimientos:: [Move Natural]-> [Move Natural]-> [Move Natural]
listamovimientos movimientosFA1 movimientosFA2=  movimientosFA1 ++ movimientosFA2

mapaestados:: [Natural]-> Natural -> Map Natural Natural
mapaestados [] cont = Map.empty 
mapaestados (x:xs) cont = Map.insert x cont (mapaestados xs cont+1)

movimientosconj:: [Move Natural] -> Map Natural Natural -> [Move Natural]
movimientosconj [] mapa =[]
movimientosconj (x:xs) mapa=  [movimientoselem x mapa ] ++ (movimientosconj xs mapa )

movimientoselem :: Move Natural -> Map Natural Natural -> Move Natural
movimientoselem (Move estado1 char estado2) mapa = Move (mapa Map.! estado1) char (mapa Map.! estado2)








    
