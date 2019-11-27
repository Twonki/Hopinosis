module Core.Metric where 

import Core.Path 
import Core.Graph
import Core.Node   

import qualified Data.Map.Monoidal.Strict as Map
import Data.Text (Text(..))

type Metric = Path -> Double
type GraphAwareMetric = Graph -> Path -> Double

edgeStrengths :: Metric
edgeStrengths [] = 0.0
edgeStrengths [x] = 0.0
edgeStrengths p@(x:y:xs)= (fromIntegral $ nextStrength p) + edgeStrengths (y:xs)
    where 
        nextStrength :: Path -> Int 
        nextStrength [] = 0
        nextStrength [x] = 0
        nextStrength (x:y:xs) = (outs $ snd x) Map.! (fst y)

averagedEdgeStrengths :: Metric
averagedEdgeStrengths p = edgeStrengths p / (fromIntegral $ length p)

magnitudes :: Metric 
magnitudes p = fromIntegral (sum $ (magnitude .snd) <$> p)

averagedMagnitudes :: Metric 
averagedMagnitudes p = magnitudes p / (fromIntegral $ length p)
