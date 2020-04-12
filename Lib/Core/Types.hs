{-|
Module      : Core.Types
Description : Types needed by several parts of the library gathered in one file
License     : MIT
Maintainer  : Leonhard.Applis@Protonmail.com

Incubator for most type definitions and data types. 
-}
module Core.Types where 

import Data.Text(Text(..)) 
import qualified Data.Map.Monoidal.Strict as Map
import Core.Node

-- | Node is used in Paths.
--
-- Node is not used for Graphs!
type Node = (Text,Values)

-- | The Edges are imminent in the outs of values
type Graph = Map.MonoidalMap Text Values
-- | A simple path - out nodes will be kept even though not used. 
type Path = [Node]
-- | Metric to calculate the value of a graph
type Metric = Path -> Double
-- | A metric which needs the graph
type GraphAwareMetric = Graph -> Path -> Double
-- | A function which tells the similarity of two paths
type DistanceFunction = Path -> Path -> Double
