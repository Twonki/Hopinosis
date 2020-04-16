{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment

import qualified Hopinosis as Hopi
import qualified Core.Metric as HopiMetric
import GHC.Conc(numCapabilities)
import qualified Data.Text.IO as TxtIO
import qualified Data.Text as Txt
import Control.DeepSeq
import Control.Exception (evaluate)

import Data.Time 

import Control.Monad(when)

import Options.Applicative
import Data.Semigroup ((<>))

-- The way to parse arguments is taken from 
-- https://hackage.haskell.org/package/optparse-applicative
-- Which is a proper library 
-- This is just a variation of the example given, so you might be enlightend visiting their documentation

data Arguments = Arguments
  { fpath :: String
  , verbose :: Bool
  , n :: Int 
  , delta :: Double
  , theta :: Double
  , sim :: String}

args' :: Parser Arguments
args' = Arguments
      <$> strOption
          ( long "filepath"
         <> short 'f'
         <> metavar "STRING"
         <> help "Filepath to the .txt to summarize (relative or absolute)" )
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
    <*> strOption
          ( long "sim"
         <> short 's'
         <> help "similiarity metric for finding distinct summaries- currently either 'jaccard' or 'cosine'"
         <> showDefault
         <> value "jaccard"
         <> metavar "STRING" )

main :: IO ()
main = summarize =<< execParser opts 
    where 
        opts = info (args' <**> helper) (fullDesc<> progDesc "TODO: add after-usage-description"<> header "TODO: Add Toplevel description")

summarize :: Arguments -> IO ()
summarize args = do
    when (verbose args) (printArgs args)

    when (verbose args) (printTimestamp "Start")

    file <- TxtIO.readFile $ fpath args
    evaluate (rnf file)
    when (verbose args) (printTimestamp "Fileread done")

    let g = Hopi.toGraphSentences file
    evaluate (rnf g)
    when (verbose args) (printTimestamp "Graphbuilding done")
    
    let results =  useExistingGraph args g
    evaluate (rnf results)
    when (verbose args) (printTimestamp "Results done")

    TxtIO.putStrLn $ Txt.unlines results

printArgs :: Arguments -> IO ()
printArgs args = do 
    putStrLn "Running Hopinosis App with"
    putStrLn $ "filepath: " ++ fpath args
    putStrLn ( "number of cores: " ++ show numCapabilities )
    putStrLn $ "using" ++ show (sim args) ++ "-similiarity"
    putStrLn "using averaged edge-strength (hardcoded)"
    putStrLn $ "Delta: "++ show (delta args) ++ " Theta: " ++ show (theta args)
    putStrLn ""


--resolveSim :: String -> HopiMetric.Distance 
resolveSim "jaccard" = HopiMetric.jaccardSim
resolveSim "cosine" = HopiMetric.cosineSim
resolveSim _ = \x y->1 -- otherwise const function

useExistingGraph args g = Hopi.summarizeFrom HopiMetric.averagedEdgeStrengths (resolveSim $ sim args ) (fromIntegral $ n args) (theta args) (delta args) (\x->x) g

printTimestamp :: String -> IO ()
printTimestamp comment = do 
    t <- getCurrentTime
    putStrLn $ comment ++ " at"
    print t
    return ()