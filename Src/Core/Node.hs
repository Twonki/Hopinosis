module Core.Node(Node,Values(..),setStart) where 
                            
import qualified Data.Map.Monoidal.Strict as Map
import Data.Text

type Node = (Text,Values)

data Values = Values {
    magnitude::Int,
    outs::Map.MonoidalMap Text Int,
    starts::Int,
    validEnd::Bool
    } deriving (Show,Eq,Ord)

setStart:: Values -> Values
setStart (Values i o _ e) = (Values i o 1 e)

merge :: Values -> Values -> Values
merge (Values i o s e) (Values i2 o2 s2 e2) = 
    Values (i+i2) (Map.unionWith (+) o o2) (s+s2) (e||e2)

emptyValues = Values 0 Map.empty 0 False

instance Semigroup Values where
    (<>) = merge

instance Monoid Values where 
    mempty = emptyValues