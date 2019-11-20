module Core.Path where

import Core.Graph
import Core.Node

import Data.Text hiding (map,foldr)
-- Paths 
-- Filter Cyclic Paths if Key is already in Path
type Path = [Node]

isValid x = isValidStarted  x && isValidEnded  x && isAsyclic x

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

isAsyclic = not . isCyclic

-- Make Step by Multyplying every out key 
-- Filter by Cyclic
-- Repeat till done
    