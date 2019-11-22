module Core.Path.Internals where

import Core.Graph
import Core.Node

import qualified Data.Map.Monoidal.Strict as Map
import Data.Text hiding (map,foldr,filter,concat)

import qualified Data.Set as Set


type Path = [Node]

{-
    Path Creation from Graph
-}

lookupOuts :: Text -> Graph -> [Node]
lookupOuts t g = filter (\(x,_) -> x == t ) $ Map.assocs g 

allPaths :: Graph -> [Path]
allPaths g = filter isValid $ (unique $ allPathsRecursive startPaths g)
    where 
        startPaths = (\s -> [s]) <$> starts g
        allPathsRecursive ps g = 
            let ps' = nextPaths ps g
            in 
                if ps' == ps || ps' == []
                then ps 
                else ps ++ allPathsRecursive ps' g
                
unique = Set.toList . Set.fromList

starts :: Graph -> [Node]
starts = filter (\(k,v) -> validStart v) . Map.assocs

nextPaths :: [Path] -> Graph -> [Path]
nextPaths paths g= mconcat (filter isAcyclic <$> (\x -> nextPathsHelper x g ) <$> paths)
    where 
        nexts :: Node -> Graph -> [Node]
        nexts (_,Values _ os _ _) g =  mconcat $ map (\k -> lookupOuts k g) (Map.keys os)

        nextPathsHelper :: Path-> Graph -> [Path]
        nextPathsHelper [] g = []
        nextPathsHelper [x] g = map (\p -> x:[p]) $ nexts x g
        nextPathsHelper (x:xs) g = map (\p ->x:p) $ nextPathsHelper xs g


{-
    Validity of Paths
-}

isValid :: Path -> Bool 
isValid x = isValidStarted  x && isValidEnded  x && isAcyclic x

isValidStarted :: Path -> Bool
isValidStarted ((_,Values _ _ True _):_) = True
isValidStarted _ = False

isValidEnded :: Path -> Bool
isValidEnded [] = False
isValidEnded [(_,Values _ _ _ True)] = True
isValidEnded (x:xs) = isValidEnded xs

isInPath :: Text -> Path -> Bool 
isInPath s xs = foldr (||) False $ map (\(x,_) -> x==s) xs

isCyclic :: Path -> Bool 
isCyclic [] = False
isCyclic [x] = False
isCyclic ((x,_):ps) = isInPath x ps || isCyclic ps

isAcyclic = not . isCyclic

-- Make Step by Multiplying every out key 
-- Filter by Cyclic
-- Repeat till done
    