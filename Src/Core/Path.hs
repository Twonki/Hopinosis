module Core.Path(
    isValid,
    validStarts,
    allPaths,
    isValidWithSigmaAlpha,
    validStartsWithSigmaAlpha,
    ) 
where
import qualified Core.Path.Internals as Internals
import Core.Graph
import Core.Node


type Path = Internals.Path

validStartsWithSigmaAlpha :: Double -> Graph -> [Path]
validStartsWithSigmaAlpha = Internals.validStartsWithSigmaAlpha

isValidWithSigmaAlpha :: Double -> Path -> Bool 
isValidWithSigmaAlpha = Internals.isValidWithSigmaAlpha

isValid :: Path -> Bool
isValid = Internals.isValid

validStarts :: Graph -> [Path]
validStarts = Internals.validStarts

allPaths :: Graph -> [Path]
allPaths = Internals.allPaths