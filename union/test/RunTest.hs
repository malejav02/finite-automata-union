import Union
import Language.Mira.FA.Types
import Language.Mira.FA.Implement(accepts)
import Data.Set (Set,toList,singleton,size,elems)
import qualified Data.Set as Set
import Data.Maybe (Maybe(Nothing))
import Data.Map (Map,insert,empty)
import qualified Data.Map as Map
import Numeric.Natural (Natural)
import Test.QuickCheck
    ( Arbitrary(arbitrary)
    , quickCheck
    ,arbitrarySizedNatural
    ,shrink
    ,shrinkIntegral
    ,withMaxSuccess 
    )

instance Arbitrary Natural where
        
    arbitrary = arbitrarySizedNatural --gives to arbitrary a Natural Value
    shrink  = shrinkIntegral

main :: IO ()
main = quickCheck $ (withMaxSuccess 1000 test)

-- |The test function receives a word and two automata and returns a boolean. If the boolean 
-- is true, means that the word is accepted by the language recognised by each automata and
-- their union. In this function is used the union definition (||).
test :: String 
           -> FA Natural
           -> FA Natural
           -> Bool
test  word fa1 fa2 = ((accepts (union fa1 fa2) word)  == (accepts fa1 word) || (accepts fa2 word))
