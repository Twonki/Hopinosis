module Core.Path.Internals where

import Core.Graph
import Core.Node

import qualified Data.Map.Monoidal.Strict as Map
import Data.Text(Text(..))
import qualified Data.Set as Set


type Path = [Node]

{- Path Creation from Graph -}

lookupOuts :: Text -> Graph -> [Node]
lookupOuts t g = filter (\(x,_) -> x == t ) $ Map.assocs g 

allPathsWithSigmaAlpha :: Double -> Graph -> [Path]
allPathsWithSigmaAlpha f g = filter (isValidWithSigmaAlpha f) $ (unique $ allPathsRecursive startPaths g)
    where 
        startPaths = validStartsWithSigmaAlpha f g
        allPathsRecursive ps g = 
            let ps' = nextPaths ps g
            in 
                if ps' == ps || ps' == []
                then ps 
                else ps ++ allPathsRecursive ps' g
        unique = Set.toList . Set.fromList

validStartsWithSigmaAlpha :: Double -> Graph -> [Path]
validStartsWithSigmaAlpha f g= (map (\s -> [s]) . filter (isValidStartedWithSigmaAlpha f)) (Map.assocs g)

nextPaths :: [Path] -> Graph -> [Path]
nextPaths paths g= mconcat (filter isAcyclic <$> (\x -> nextPathsHelper x g ) <$> paths)
    where 
        nexts :: Node -> Graph -> [Node]
        nexts (_,Values _ os _ _) g =  mconcat $ map (\k -> lookupOuts k g) (Map.keys os)
        nextPathsHelper :: Path-> Graph -> [Path]
        nextPathsHelper [] g = []
        nextPathsHelper [x] g = map (\p -> x:[p]) $ nexts x g
        nextPathsHelper (x:xs) g = map (\p ->x:p) $ nextPathsHelper xs g

{- Validity of Paths -}

isValidWithSigmaAlpha :: Double -> Path -> Bool
isValidWithSigmaAlpha f [] = False
isValidWithSigmaAlpha f p = isValidStartedWithSigmaAlpha' f p &&  isValidEnded p && isAcyclic p

isValidStartedWithSigmaAlpha :: Double -> Node -> Bool
isValidStartedWithSigmaAlpha f (_,Values m _ s _) =  ((fromIntegral s) / (fromIntegral m)) > f

isValidStartedWithSigmaAlpha' :: Double -> Path -> Bool
isValidStartedWithSigmaAlpha' f = (isValidStartedWithSigmaAlpha f) . head

isValidEnded :: Path -> Bool
isValidEnded [] = False
isValidEnded [(_,Values _ _ _ True)] = True
isValidEnded (x:xs) = isValidEnded xs

isCyclic :: Path -> Bool 
isCyclic [] = False
isCyclic [x] = False
isCyclic ((x,_):ps) = isInPath x ps || isCyclic ps
        where 
            isInPath :: Text -> Path -> Bool 
            isInPath s xs = foldr (||) False $ map (\(x,_) -> x==s) xs
            
isAcyclic = not . isCyclic