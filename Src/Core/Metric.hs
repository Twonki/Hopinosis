module Core.Metric where 

import Core.Path 
import Core.Graph
import Core.Node   

import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.Map.Strict as SMap
import Data.Text (Text(..))
import Data.Sort (sortOn)
import qualified Data.Set as Set

type Metric = Path -> Double
type GraphAwareMetric = Graph -> Path -> Double
type DistanceFunction = Path -> Path -> Double

edgeStrengths :: Metric
edgeStrengths [] = 0.0
edgeStrengths [x] = 0.0
edgeStrengths p@(x:y:xs)= (fromIntegral $ nextStrength p) + edgeStrengths (y:xs)
    where 
        nextStrength :: Path -> Int 
        nextStrength [] = 0
        nextStrength [x] = 0
        nextStrength (x:y:xs) = (outs $ snd x) MMap.! (fst y)

averagedEdgeStrengths :: Metric
averagedEdgeStrengths p = edgeStrengths p / (fromIntegral $ length p)

magnitudes :: Metric 
magnitudes p = fromIntegral (sum $ (magnitude .snd) <$> p)

averagedMagnitudes :: Metric 
averagedMagnitudes p = magnitudes p / (fromIntegral $ length p)


toVectors :: (Path,Path) -> ([Double],[Double])
toVectors (p1,p2) = 
        (map (\w -> SMap.findWithDefault 0 w p1') unionBagOfWords, map (\w -> SMap.findWithDefault 0 w p2') unionBagOfWords)
        where 
            unique = Set.toList . Set.fromList
            unionBagOfWords = unique $ (fst <$> p1) ++ (fst <$> p2)
            p1' = SMap.fromAscList $ (\(k,v) -> (k,fromIntegral $ magnitude v)) <$> p1
            p2' = SMap.fromAscList $ (\(k,v) -> (k,fromIntegral $ magnitude v)) <$> p2

cosineSim :: Path -> Path -> Double
cosineSim [] _ = 0.0
cosineSim _ [] = 0.0
cosineSim p1 p2 = cosine p1vec p2vec
    where 
        (p1vec,p2vec) = toVectors (p1,p2)
        cosine :: [Double] -> [Double] -> Double
        cosine xs ys = dot xs ys / (len xs * len ys)
            where 
                dot a b = sum $ zipWith (*) a b
                len a = sqrt $ dot a a

{- Tuple Selection -}
bestPaths :: Metric -> DistanceFunction -> Int -> Double -> [Path] -> [Path]
bestPaths mFn distFn n theta ps = 
    let 
        candidates = validCandidatesWithLength ps n
        filteredCandidates = filterBySigmaTheta candidates mFn theta
        sortedCandidates = sortByDistances distFn filteredCandidates
    in
        getFirst sortedCandidates
    where 
        getFirst :: [[Path]] -> [Path]
        getFirst [] = []
        getFirst (x:_) = x

commonBestPaths :: [Path] -> [Path]
commonBestPaths = bestPaths averagedEdgeStrengths cosineSim 3 0.25

validCandidatesWithLength :: [Path] -> Int -> [[Path]]
validCandidatesWithLength ps n = uniqueCandidates (candidatesWithLength n ps)
    where
        candidatesWithLength :: Int -> [Path] -> [[Path]]
        candidatesWithLength 0 _ = []
        candidatesWithLength 1 xs = (\x -> [x]) <$> xs
        candidatesWithLength n ps = mconcat $ (\recPs -> ( map (\p -> p:recPs)) ps) <$> (candidatesWithLength (n-1) ps)
        uniqueCandidates :: [[Path]] -> [[Path]]
        uniqueCandidates = (map Set.toList) . Set.toList . Set.fromList . (map Set.fromList)

calculateAllDistances :: DistanceFunction -> [Path] -> [Double]
calculateAllDistances _ [] = []
calculateAllDistances _ (x:[]) = [0.0]
calculateAllDistances dist (x:os) = map (dist x) os ++ calculateAllDistances dist os

calculateAllMetrics :: Metric -> [Path] -> [Double]
calculateAllMetrics mfn = map mfn

filterBySigmaTheta :: [[Path]] -> Metric -> Double -> [[Path]]
filterBySigmaTheta ps mfn theta = filter (\x -> sigmaThetaQualified x mfn theta) ps
    where 
        sigmaThetaQualified :: [Path] -> Metric -> Double -> Bool
        sigmaThetaQualified p mfn sigma= product (map mfn p) > sigma

sortByDistances :: DistanceFunction -> [[Path]] -> [[Path]]
sortByDistances distFn ps = sortOn (calculateAllDistances distFn) ps