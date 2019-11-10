module Core.Node where 
                            
type Node = (String,Values)    
data Values = Values {
    magnitude::Int,
    outs::[(String,Int)],
    validStart::Bool,
    validEnd::Bool
}

(<>) :: Values -> Values -> Values 
(<>) = undefined


setStart :: Node -> Node 
setStart (k, Values i o _ e) = (k , Values i o True e)