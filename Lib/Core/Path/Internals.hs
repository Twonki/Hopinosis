{-|
Module      : Core.Path.Internals
Description : Internal class for creating and validating paths from an opinosis graph
License     : MIT
Maintainer  : Leonhard.Applis@Protonmail.com
-}
module Core.Path.Internals where

import Core.Node
import Core.Types

import qualified Data.Map.Monoidal.Strict as Map
import Data.Text(Text)
import qualified Data.Set as Set
import Data.Monoid(Any(..),Sum(..))

-- * Path Creation from Graph
--
-- $PathCreation
-- The following functions are centered about creating paths from a graph. 
-- 
-- The top-level function is allPathsWithSigmaAlpha, everything else is just providing help for it. 

-- | Looks up a node in a graph, and returns all outgoing nodes which are found in the graph
lookupOuts :: Text -> Graph -> [Node]
lookupOuts t g = filter (\(x,_) -> x == t ) $ Map.assocs g 

-- | Returns all potential valid paths of a graph given a certain sigmaAlpha.
-- 
-- The paths are being checked to start with only valid nodes given the sigmaAlpha, 
-- are not cyclic and are valid ended. 
allPathsWithSigmaAlpha :: Double -> Graph -> [Path]
allPathsWithSigmaAlpha f g = filter (isValidWithSigmaAlpha f) (unique $ allPathsRecursive startPaths g)
    where 
        startPaths = validStartsWithSigmaAlpha f g
        allPathsRecursive ps graph = 
            let ps' = nextPaths ps graph
            in 
            if ps' == ps || null ps'
                then ps 
                else ps ++ allPathsRecursive ps' graph
        unique = Set.toList . Set.fromList

-- | Returns all nodes in a graph, which are valid starts given a certain sigmaAlpha.
-- 
-- The nodes are wrapped into a single-element list, so they are single-node paths.
-- The paths are then used as starting points for other functions.
validStartsWithSigmaAlpha :: Double -> Graph -> [Path]
validStartsWithSigmaAlpha f g= (map (:[]) . filter (isValidStartedWithSigmaAlpha f)) (Map.assocs g)

-- | Returns all potential paths following the current path in a graph. 
-- 
-- given the node b has two outs, called c1 and c2, this function will give:
-- 
-- @
-- nextPaths [[a,b]] g = [[a,b,c1],[a,b,c2]]
-- @
-- 
-- Where c1 and c2 have to be in the graph.
-- 
-- The paths are checked for being acyclic. 
nextPaths :: [Path] -> Graph -> [Path]
nextPaths paths g= mconcat (filter isAcyclic . (`nextPathsHelper` g ) <$> paths)
    where 
        nexts :: Node -> Graph -> [Node]
        nexts (_,Values _ os _ _) v =  mconcat $ map (`lookupOuts` v) (Map.keys os)
        nextPathsHelper :: Path-> Graph -> [Path]
        nextPathsHelper [] _ = []
        nextPathsHelper [x] u = map (\p -> x:[p]) $ nexts x u
        nextPathsHelper (x:xs) u = map (x:) $ nextPathsHelper xs u

-- * Path Validation
--
-- $PathValidation
-- The following functions are checking a path for being valid. 
-- 
-- The primary function is isValidWithSigmaAlpha, everything else is just providing for it.

-- | check whether a path is both valid Ended and valid started given a certain sigmaAlpha
isValidWithSigmaAlpha :: Double -> Path -> Bool
isValidWithSigmaAlpha _ [] = False
isValidWithSigmaAlpha f p = isValidStartedWithSigmaAlpha' f p &&  isValidEnded p && isAcyclic p

-- | Validates whether a node is a valid start given a certain sigmaAlpha
isValidStartedWithSigmaAlpha :: Double -> Node -> Bool
isValidStartedWithSigmaAlpha f (_,Values (Sum m) _ (Sum s) _) =  (fromIntegral s / fromIntegral m) > f

-- | Checks wether the first node of a path is a valid start given a certain sigmaAlpha
isValidStartedWithSigmaAlpha' :: Double -> Path -> Bool
isValidStartedWithSigmaAlpha' f = isValidStartedWithSigmaAlpha f . head

-- | Validates whether a path ends with a node which is an end-node
isValidEnded :: Path -> Bool
isValidEnded [] = False
isValidEnded [(_,Values _ _ _ (Any True))] = True
isValidEnded (_:xs) = isValidEnded xs

-- | Checks wether a path contains a node twice. 
-- 
-- This is done recursive via comparing if a text is occurring twice. 
isCyclic :: Path -> Bool 
isCyclic [] = False
isCyclic [_] = False
isCyclic ((x,_):ps) = isInPath x ps || isCyclic ps
        where 
            isInPath :: Text -> Path -> Bool 
            isInPath s = foldr ((||) . \(f,_) -> f==s) False

            
-- | isAcyclic = not isCyclic
-- 
-- no additional logic.
isAcyclic :: Path -> Bool       
isAcyclic = not . isCyclic