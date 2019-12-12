{-|
Module      : Core.Path
Description : Functions to build and evaluate paths from a opinosis graph
License     : MIT
Maintainer  : Leonhard.Applis@Protonmail.com

Most of the functions are implemented in "Core.Path.Internals"
-}
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

-- |interface to "Core.Path.Internals"
validStartsWithSigmaAlpha :: Double -- ^sigma alpha value - threshold how often 
    -> Graph -- ^The graph where the starts will be looked up
    -> [Path] -- ^all Nodes which are valid starts given the sigma alpha, wrapped into a list (to be a single node path)
validStartsWithSigmaAlpha = Internals.validStartsWithSigmaAlpha

-- |Returns every Node of the graph which has been atleast once been a start
-- Closure of validStartsWithSigmaAlpha 0.0
validStarts :: Graph -> [Path]
validStarts = validStartsWithSigmaAlpha 0.0

-- |interface to "Core.Path.Internals"
isValidWithSigmaAlpha :: Double -> Path -> Bool 
isValidWithSigmaAlpha = Internals.isValidWithSigmaAlpha

-- | isValidWithSigmaAlpha 0.0
isValid :: Path -> Bool
isValid = isValidWithSigmaAlpha 0.0


allPathsWithSigmaAlpha :: Double -> Graph -> [Path]
allPathsWithSigmaAlpha = Internals.allPathsWithSigmaAlpha

allPaths :: Graph -> [Path]
allPaths = allPathsWithSigmaAlpha 0.0