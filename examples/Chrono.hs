-- | A simple example of how to use Chronograph.  We want to count the lines in
-- a file, and measure how long it takes.

module Main where

import Control.Applicative
import System.Environment
import Data.Chronograph

import Text.Printf

main :: IO ()
main = do
    args <- getArgs
    putStrLn "pure Chronograph"
    mapM_ procFile args
    putStrLn "IO Chronograph"
    mapM_ procIO args

procFile :: FilePath -> IO ()
procFile fp = do
    doc <- readFile fp
    let wc = length $ lines doc
    putStrLn $ formatOutput fp (chrono wc)
    
formatOutput :: FilePath -> Chronograph Int -> String
formatOutput fp chr = printf "%s :: %d, %s" fp (val chr) (show $ measure chr)

fileLinesIO :: FilePath -> IO Int
fileLinesIO fp = length . lines <$> readFile fp

procIO :: FilePath -> IO ()
procIO fp = do
  wc <- chronoIO $ fileLinesIO fp
  putStrLn $ formatOutput fp wc
