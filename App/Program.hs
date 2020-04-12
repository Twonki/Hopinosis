{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment

import qualified Hopinosis as Hopi
import qualified Core.Metric as HopiMetric
import GHC.Conc(numCapabilities)
import qualified Data.Text.IO as TxtIO
import qualified Data.Text as Txt
import Control.DeepSeq


import Data.Time 


main :: IO ()
main = do 
    [f,n,theta,delta] <- getArgs
    let n' = read n :: Word
    let theta' = read theta :: Double
    let delta' = read delta :: Double

    putStrLn $ "number of cores: " ++ show numCapabilities
    putStrLn $ "looking for a summary of " ++ show n' ++ " sentences"
    putStrLn $ "using jaccard-sim and averadged edge-strength (hardcoded)"
    putStrLn $ "Delta: "++ show delta' ++ " Theta: " ++ show theta'
    
    putStrLn  "service alive and healthy \n"

    !file <- TxtIO.readFile f

    putStrLn "file read - starting opinosis" 

    inittime <- getCurrentTime
    putStrLn $ show inittime

    let g = force $ Hopi.toGraphSentences file
    graphtime <- getCurrentTime
    putStrLn $ "Graph read done @" ++ show graphtime
    putStrLn $ "Time for graph: " ++ show (diffUTCTime graphtime inittime)

    let !results =  useExistingGraph n' theta' delta' g
    TxtIO.putStrLn $ Txt.unlines results


    donetime <- getCurrentTime
    putStrLn $ show donetime

    let difference = diffUTCTime donetime inittime
    putStrLn $ "comparison-time:" ++ show (diffUTCTime donetime graphtime)
    putStrLn $ "time elapsed:"++show difference

--resolveSim :: String -> HopiMetric.Distance 
resolveSim "jaccard" = HopiMetric.jaccardSim
resolveSim "cosine" = HopiMetric.cosineSim
resolveSim _ = \x y->1 -- otherwise const function

useExistingGraph n theta delta g = Hopi.summarizeFrom HopiMetric.averagedEdgeStrengths HopiMetric.jaccardSim n theta delta (\x->x) g