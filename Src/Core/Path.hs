module Core.Path(
    isValid,
    starts,
    allPaths
    ) 
where
import qualified Core.Path.Internals as Internals
import Core.Graph
import Core.Node

type Path = Internals.Path

isValid :: Path -> Bool
isValid = Internals.isValid

starts :: Graph -> [Node]
starts = Internals.starts

allPaths :: Graph -> [Path]
allPaths = Internals.allPaths