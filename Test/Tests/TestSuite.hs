module Tests.TestSuite(
    module Core.Graph,
    module Core.Node,
    module Core.Path,
    uniValue,
    startValue,
    endValue
    )
where 
import Core.Graph
import Core.Node
import Core.Path
import qualified Data.Map.Monoidal.Strict as Map


uniValue   = Values 1 Map.empty False False
startValue = Values 1 Map.empty True False 
endValue   = Values 1 Map.empty False True