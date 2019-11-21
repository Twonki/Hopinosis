module Core.Path.Internals where

import Core.Graph
import Core.Node

import qualified Data.Map.Monoidal.Strict as Map
import Data.Text hiding (map,foldr,filter)


type Path = [Node]

{-
    Path Creation from Graph
-}

lookupOuts :: Text -> Graph -> [Node]
lookupOuts t g = filter (\(x,_) -> x == t ) $ Map.assocs g 

nexts :: Node -> Graph -> [Node]
nexts (_,Values _ os _ _) g =  mconcat $ map (\k -> lookupOuts k g) (Map.keys os)

nextPaths :: Path-> Graph -> [Path]
nextPaths [] g = []
nextPaths [x] g = map (\p -> x:[p]) $ nexts x g
nextPaths (x:xs) g = map (\p ->x:p) $ nextPaths xs g

starts :: Graph -> [Node]
starts = filter (\(k,v) -> validStart v) . Map.assocs

--firstStep g = filter isAcyclic $ map (\x -> nextPaths x g) $  [starts g]
--secondStep g = filter isAcyclic $ map nextPaths $ firstStep g
{-
    Validity of Paths
-}

isValid :: Path -> Bool 
isValid x = isValidStarted  x && isValidEnded  x && isAcyclic x

isValidStarted :: Path -> Bool
isValidStarted ((_,Values _ _ True _):_) = True
isValidStarted _ = False

isValidEnded :: Path -> Bool
isValidEnded [(_,Values _ _ _ True)] = True 
isValidEnded _ = False

isInPath :: Text -> Path -> Bool 
isInPath s xs = foldr (||) False $ map (\(x,_) -> x==s) xs

isCyclic :: Path -> Bool 
isCyclic ((x,_):ps) = isInPath x ps || isCyclic ps

isAcyclic = not . isCyclic

-- Make Step by Multiplying every out key 
-- Filter by Cyclic
-- Repeat till done
    