module Core.Node where 
                            
type Node = (String,Values)    
data Values = Values {
    magnitude::Int,
    outs::[(String,Int)],
    validStart::Bool,
    validEnd::Bool
    }

emptyValues = Values 0 [] False False 

valueMerge :: Values -> Values -> Values
-- Combine the inner values, || for the booleans
valueMerge (Values i o s e) (Values i2 o2 s2 e2) = Values (i+i2) (o++o2) (s||s2) (e||e2)

foldValues :: [Values] -> Values
foldValues vs = foldr valueMerge emptyValues vs 

setStart :: Node -> Node 
setStart (k, Values i o _ e) = (k , Values i o True e)