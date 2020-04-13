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
        opts = info (args' <**> helper) (fullDesc<> progDesc "TODO: add after-usage-description"<> header "TODO: Add Toplevel description")

summarize :: Arguments -> IO ()
summarize args = do
    when (verbose args) (printArgs args)

    !file <- TxtIO.readFile $ fpath args

    let g = force $ Hopi.toGraphSentences file

    let !results =  useExistingGraph (fromIntegral $ n args) (theta args) (delta args) g

    TxtIO.putStrLn $ Txt.unlines results

printArgs :: Arguments -> IO ()
printArgs args = do 
    putStrLn "Running Hopinosis App with"
    putStrLn $ "filepath: " ++ fpath args
    putStrLn ( "number of cores: " ++ show numCapabilities )
    putStrLn "using jaccard-sim and averadged edge-strength (hardcoded)"
    putStrLn $ "Delta: "++ show (delta args) ++ " Theta: " ++ show (theta args)


--resolveSim :: String -> HopiMetric.Distance 
resolveSim "jaccard" = HopiMetric.jaccardSim
resolveSim "cosine" = HopiMetric.cosineSim
resolveSim _ = \x y->1 -- otherwise const function




useExistingGraph n theta delta g = Hopi.summarizeFrom HopiMetric.averagedEdgeStrengths HopiMetric.jaccardSim n theta delta (\x->x) g