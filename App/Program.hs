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

import Options.Applicative
import Data.Semigroup ((<>))

-- The way to parse arguments is taken from 
-- https://hackage.haskell.org/package/optparse-applicative
-- Which is a proper library 

data Arguments = Arguments
  { fpath :: String
  , verbose :: Bool
  , n :: Int 
  , delta :: Double
  , theta :: Double}

args' :: Parser Arguments
args' = Arguments
      <$> strOption
          ( long "filepath"
         <> short 'f'
         <> metavar "fpath"
         <> help "Filepath to the .txt to summarize" )
      <*> switch
          ( long "verbose"
         <> short 'v'
         <> help "whether to print debug information" )
      <*> option auto
          ( long "summaries"
         <> short 'n'
         <> help "number of summary-sentences to be generated"
         <> showDefault
         <> value 1
         <> metavar "INT" )
    <*> option auto
          ( long "delta"
         <> short 'd'
         <> help "delta threshold for the opinosis algorithm"
         <> showDefault
         <> value 0.51
         <> metavar "DOUBLE" )
    <*> option auto
          ( long "theta"
         <> short 't'
         <> help "theta threshold for the opinosis algorithm"
         <> showDefault
         <> value 0.51
         <> metavar "DOUBLE" )

main :: IO ()
main = summarize =<< execParser opts 
    where 
        opts = info (args' <**> helper) (fullDesc<> progDesc "Print a greeting for TARGET"<> header "hello - a test for optparse-applicative")

summarize :: Arguments -> IO () 
summarize _ = print "Kekw"


{-- 
    [f,n,theta,delta] <- getArgs
    let n' = read n :: Word
    let theta' = read theta :: Double
    let delta' = read delta :: Double

    putStrLn $ "number of cores: " ++ show numCapabilities
    putStrLn $ "looking for a summary of " ++ show n' ++ " sentences"
    putStrLn "using jaccard-sim and averadged edge-strength (hardcoded)"
    putStrLn $ "Delta: "++ show delta' ++ " Theta: " ++ show theta'
    
    putStrLn  "service alive and healthy \n"

    !file <- TxtIO.readFile f

    putStrLn "file read - starting opinosis" 

    inittime <- getCurrentTime
    print inittime

    let g = force $ Hopi.toGraphSentences file
    graphtime <- getCurrentTime
    putStrLn $ "Graph read done @" ++ show graphtime
    putStrLn $ "Time for graph: " ++ show (diffUTCTime graphtime inittime)

    let !results =  useExistingGraph n' theta' delta' g
    TxtIO.putStrLn $ Txt.unlines results


    donetime <- getCurrentTime
    print donetime

    let difference = diffUTCTime donetime inittime
    putStrLn $ "comparison-time:" ++ show (diffUTCTime donetime graphtime)
    putStrLn $ "time elapsed:"++show difference
--}

--resolveSim :: String -> HopiMetric.Distance 
resolveSim "jaccard" = HopiMetric.jaccardSim
resolveSim "cosine" = HopiMetric.cosineSim
resolveSim _ = \x y->1 -- otherwise const function

useExistingGraph n theta delta g = Hopi.summarizeFrom HopiMetric.averagedEdgeStrengths HopiMetric.jaccardSim n theta delta (\x->x) g