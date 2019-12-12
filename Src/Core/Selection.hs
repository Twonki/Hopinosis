module Core.Selection(bestPaths,commonBestPaths) where 
import qualified Core.Selection.Internals as Internals
import qualified Core.Metric as Metric
import Core.Path


bestPaths = Internals.bestPaths

commonBestPaths :: [Path] -> [Path]
commonBestPaths = bestPaths Metric.averagedEdgeStrengths Metric.cosineSim 3 1.25