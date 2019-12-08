module Core.Types where 

import Data.Text(Text(..)) 
import qualified Data.Map.Monoidal.Strict as Map
import Core.Node


type Node = (Text,Values)
type Graph = Map.MonoidalMap Text Values

type Path = [Node]

type Metric = Path -> Double
type GraphAwareMetric = Graph -> Path -> Double
type DistanceFunction = Path -> Path -> Double
