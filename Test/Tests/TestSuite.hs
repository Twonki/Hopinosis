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


uniValue   = Values 1 [] False False
startValue = Values 1 [] True False 
endValue   = Values 1 [] False True