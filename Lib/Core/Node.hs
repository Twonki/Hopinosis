{-|
Module      : Core.Node
Description : Contains the Values of a single node of the opinosis graph
License     : MIT
Maintainer  : Leonhard.Applis@Protonmail.com

This module contains the "Values" of a single node. 

The values are declared a monoid and a merge function is provided.
-}
module Core.Node(Values(..),setStart) where 
import qualified Data.Map.Monoidal.Strict as Map
import Data.Text(Text(..))
import Data.Monoid(Sum(..),Any(..))
import Control.DeepSeq (NFData,rnf)

-- | Data Type Values
-- Used to describe the values of a word found in the opinosis graph
-- All the attributes are itself monoidal
data Values = Values {
     -- | the number of occurrences 
    magnitude::Sum Word,
     -- | the outgoing edges and their edge strength
    outs::Map.MonoidalMap Text (Sum Word),
     -- | the number of occurrences where this node has been a start
    starts::Sum Word,
     -- | whether this node has been an end of a sentence at least once
    validEnd::Any
    } deriving (Show,Eq,Ord)

-- | Sets the value "start" to one
setStart:: Values -> Values
setStart (Values i o _ e) = Values i o 1 e

-- | merges every value of Values according to their individual monoidal instance
merge :: Values -> Values -> Values
merge (Values i o s e) (Values i2 o2 s2 e2) = 
    Values (i<>i2) (o <> o2) (s <> s2) (e <> e2)

-- | Neutral element, used for mempty and especially for tests
emptyValues :: Values
emptyValues = Values mempty Map.empty mempty mempty

instance Semigroup Values where
    (<>) = merge

instance Monoid Values where 
    mempty = emptyValues

-- | NFData enables to "force evaluate" the Values, 
--   that is to forcefully break the laziness.
--   It is implemented as simply forcing all components.
instance NFData Values where 
    rnf (Values m o s e) =  rnf m `seq` rnf o `seq` rnf s `seq` rnf e