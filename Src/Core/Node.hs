{-|
Module      : Core.Node
Description : Contains the Values of a single node of the opinosis graph
License     : MIT
Maintainer  : Leonhard.Applis@Protonmail.com

This module contains the "Values" of a single node. 
The values are declared a monoid and a merge function is provided.
-}
module Core.Node(Node,Values(..),setStart) where 
                            
import qualified Data.Map.Monoidal.Strict as Map
import Data.Text

type Node = (Text,Values)

-- | Data Type Values
-- All the attributes are itself monoidal
-- Magnitude is the number of occurrences 
-- Outs are the outgoing edges and their edge strength
-- starts is the number of occurrences where this node has been a start
-- Valid end marks, whether this node has been an end at least once

data Values = Values {
    magnitude::Int,
    outs::Map.MonoidalMap Text Int,
    starts::Int,
    validEnd::Bool
    } deriving (Show,Eq,Ord)

-- | Sets the value "start" to one
setStart:: Values -> Values
setStart (Values i o _ e) = (Values i o 1 e)

-- | merges every value of Values according to their individual monoidal instance
merge :: Values -> Values -> Values
merge (Values i o s e) (Values i2 o2 s2 e2) = 
    Values (i+i2) (Map.unionWith (+) o o2) (s+s2) (e||e2)

-- | Neutral element, used for mempty and especially for tests
emptyValues = Values 0 Map.empty 0 False

instance Semigroup Values where
    (<>) = merge

instance Monoid Values where 
    mempty = emptyValues