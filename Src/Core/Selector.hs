module Core.Selector(bestPaths,commonBestPaths) where 
import qualified Core.Selector.Internals as Internals
import qualified Core.Metric as Metric
import Core.Path


bestPaths = Internals.bestPaths

commonBestPaths :: [Path] -> [Path]
commonBestPaths = bestPaths Metric.averagedEdgeStrengths Metric.cosineSim 3 1.25