module Core.Path where

import Core.Graph
import Core.Node

-- Paths 
-- Filter Cyclic Paths if Key is already in Path
type Path = [Node]

-- Make Step by Multyplying every out key 
-- Filter by Cyclic
-- Repeat till done
    