module Core.Path(
    Path,
    isValid,
    validStarts,
    allPaths,
    isValidWithSigmaAlpha,
    validStartsWithSigmaAlpha,
    allPathsWithSigmaAlpha
    ) 
where
    
import qualified Core.Path.Internals as Internals
import Core.Graph
import Core.Node
import Core.Types

validStartsWithSigmaAlpha :: Double -> Graph -> [Path]
validStartsWithSigmaAlpha = Internals.validStartsWithSigmaAlpha

validStarts :: Graph -> [Path]
validStarts = validStartsWithSigmaAlpha 0.0

isValidWithSigmaAlpha :: Double -> Path -> Bool 
isValidWithSigmaAlpha = Internals.isValidWithSigmaAlpha

isValid :: Path -> Bool
isValid = isValidWithSigmaAlpha 0.0


allPathsWithSigmaAlpha :: Double -> Graph -> [Path]
allPathsWithSigmaAlpha = Internals.allPathsWithSigmaAlpha

allPaths :: Graph -> [Path]
allPaths = allPathsWithSigmaAlpha 0.0