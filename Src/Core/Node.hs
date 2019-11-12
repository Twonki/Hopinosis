module Core.Node where 
                            
type Node = (String,Values)

data Values = Values {
    magnitude::Int,
    outs::[(String,Int)],
    validStart::Bool,
    validEnd::Bool
    } deriving (Show,Eq)

emptyValues = Values 0 [] False False 

mergeValues :: Values -> Values -> Values
-- Combine the inner values, || for the booleans
mergeValues (Values i o s e) (Values i2 o2 s2 e2) = Values (i+i2) (mergeOuts o o2) (s||s2) (e||e2)
    where 
        addOut :: [(String,Int)] -> (String,Int) -> [(String,Int)]
        addOut xs d@(ky,vy) = if elem ky (map fst xs)
                                then 
                                    let hit = filter (\(k,_)-> k== ky) xs 
                                        nohits = filter (\(k,_)-> k/=ky)  xs
                                        [t@(_,vx)] = hit
                                    in (ky,vx+vy) : nohits
                                else xs++[d]
        mergeOuts :: [(String,Int)] -> [(String,Int)] -> [(String,Int)]
        mergeOuts xs [] = xs
        mergeOuts xs (y:ys) = let u = (addOut xs y) 
                              in mergeOuts u ys 

foldValues :: [Values] -> Values
foldValues vs = foldr mergeValues emptyValues vs 

setStart :: Node -> Node 
setStart (k, Values i o _ e) = (k , Values i o True e)