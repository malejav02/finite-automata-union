module Lib  where
 
import Language.Mira.FA.Types
import Numeric.Natural (Natural)
import Data.Set 
import qualified Data.Set as Set 
 
union :: FA Natural -> FA Natural -> FA Natural
union (MkFA estadosFA1 transicionesFA1 estadoInicialFA1 estadosAceptacionFA1 )
      (MkFA estadosFA2 transicionesFA2 estadoInicialFA2 estadosAceptacionFA2 )
        =   (MkFA (Set.union estadosFA1 estadosFA2)
            (Set.union transicionesFA1 transicionesFA2)
            (0)
            (Set.union estadosAceptacionFA1 estadosAceptacionFA2)) 

mover:: Set Natural -> Set Natural -> Set Natural


    
