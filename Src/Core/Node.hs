module Core.Node where 
                            
import qualified Data.Map as Map

type Node = (String,Values)
type Node2 =(String,Values2)

data Values2 = Values2 {
    magnitude2::Int,
    outs2::Map.Map String Int,
    validStart2::Bool,
    validEnd2::Bool
    } deriving (Show,Eq)

setStart2:: Node2 -> Node2
setStart2 (k, Values2 i o _ e) = (k , Values2 i o True e)

mergeValues2 :: Values2 -> Values2 -> Values2 
mergeValues2 (Values2 i o s e) (Values2 i2 o2 s2 e2) = 
    Values2 (i+i2) (mergeOuts2 o o2) (s||s2) (e||e2)
    where mergeOuts2 = Map.unionWith (+)


emptyValues2 = Values2 0 Map.empty False False

instance Semigroup Values2 where
    (<>) = mergeValues2

instance Monoid Values2 where 
    mappend = mergeValues2
    mempty = emptyValues2

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
                                    let [(_,vx)] = filter (\(k,_)-> k== ky) xs 
                                        nohits = filter (\(k,_)-> k/=ky)  xs
                                    in (ky,vx+vy) : nohits
                                else xs++[d]
        mergeOuts :: [(String,Int)] -> [(String,Int)] -> [(String,Int)]
        mergeOuts xs [] = xs
        mergeOuts xs (y:ys) = mergeOuts (addOut xs y) ys

foldValues :: [Values] -> Values
foldValues vs = foldr mergeValues emptyValues vs 

setStart :: Node -> Node 
setStart (k, Values i o _ e) = (k , Values i o True e)