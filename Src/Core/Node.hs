module Core.Node(Node,Values(..),setStart) where 
                            
import qualified Data.Map as Map

type Node = (String,Values)

data Values = Values {
    magnitude::Int,
    outs::Map.Map String Int,
    validStart::Bool,
    validEnd::Bool
    } deriving (Show,Eq)

setStart:: Values -> Values
setStart (Values i o _ e) = (Values i o True e)

merge :: Values -> Values -> Values
merge (Values i o s e) (Values i2 o2 s2 e2) = 
    Values (i+i2) (Map.unionWith (+) o o2) (s||s2) (e||e2)

emptyValues = Values 0 Map.empty False False

instance Semigroup Values where
    (<>) = merge

instance Monoid Values where 
    mempty = emptyValues