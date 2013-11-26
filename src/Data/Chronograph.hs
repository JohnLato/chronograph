{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

 -- |
 -- Measure data and IO evaluation time in a lightweight manner.
 --
 -- A 'Chronograph a' has two parts, the value 'a' and the measurement of
 -- evaluation time.  A Chronograph is lazy, so 'a' is only evaluted on demand.
 --
 -- This example counts the lines in a number of files, and records the
 -- evaluation time taken for each one.
 --
 -- >  import System.Environment
 -- >  import Control.Applicative
 -- >  import Data.Chronograph
 -- >
 -- >  procFile :: FilePath -> IO Int
 -- >  procFile fp = do
 -- >      doc <- readFile fp
 -- >      let wc = length $ lines doc
 -- >      chronoPrint "time to eval length" (chrono wc)
 --
 -- 'chrono' creates a chronograph that evaluates its input as far as 'seq' would.
 -- In this case the input 'wc' is an Int, so 'chrono' fully evaluates it.
 -- deepseq-style evaluation is performed by 'chronoNF', and custom evaluation
 -- strategies can be implemented with 'chronoBy'.
 --
 -- although 'wc' is a pure value, IO is lazily performed in its evalution.
 -- This IO cost is included in 'chrono's measurement.
 --
 -- You can explicitly include timings of IO actions as well:
 --
 -- >  fileLinesIO :: FilePath -> IO Int
 -- >  fileLinesIO fp = length . lines <$> readFile fp
 -- >
 -- >  procIO :: FilePath -> IO ()
 -- >  procIO fp = do
 -- >    wc <- chronoIO $ fileLinesIO fp
 -- >    void $ chronoPrint "fileLinesIO" wc
 --
 -- >  main :: IO ()
 -- >  main = do
 -- >      args <- getArgs
 -- >      putStrLn "pure Chronograph"
 -- >      mapM_ procFile args
 -- >      putStrLn "IO Chronograph"
 -- >      mapM_ procIO args
 --
module Data.Chronograph (
  Chronograph (..)
-- * chrono pure stuff
, chrono
, chronoNF
, chronoBy
-- * chrono IO stuff
, chronoJustIO
, chronoIO
, chronoNFIO
, chronoIOBy
-- * utility functions
, chronoPrint
, chronoTraceEvent
, chronoTraceEventIO
) where

import Control.Applicative
import Control.DeepSeq
import Data.Thyme (NominalDiffTime, getCurrentTime)
import Data.Thyme.Format.Human
import Data.AffineSpace ((.-.))
import Debug.Trace
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)

data Chronograph a = Chronograph
    { measure :: {-# UNPACK #-} !NominalDiffTime
    , val :: a
    } deriving (Show, Generic)

----------------------------------------------------------
-- chrono pure functions

-- | Add a 'Chronograph' to measure evaluation to weak head normal form.
chrono :: a -> Chronograph a
chrono = chronoBy (`seq` ())

-- | Add a 'Chronograph' to measure evaluation to normal form.
chronoNF :: NFData a => a -> Chronograph a
chronoNF = chronoBy rnf

-- | Add a 'Chronograph' to measure evalution time with the provided strategy.
chronoBy :: (a -> ()) -> a -> Chronograph a
chronoBy eval x =
    let measure = unsafePerformIO $ do
                    t0 <- getCurrentTime
                    t1 <- eval x `seq` getCurrentTime
                    return (t1 .-. t0)
    in Chronograph {val = measure `seq` x, measure }

----------------------------------------------------------
-- chrono IO computations

-- | Add a 'Chronograph' to measure IO time (no additional evaluation is
-- performed, although the IO action itself may perform some evaluation)
chronoJustIO :: IO a -> IO (Chronograph a)
chronoJustIO = chronoIOBy (const ())

-- | Add a 'Chronograph' to measure time of IO and evaluation to weak head
-- normal form.
chronoIO :: IO a -> IO (Chronograph a)
chronoIO = chronoIOBy (`seq` ())

-- | Add a 'Chronograph' to measure time of IO and evaluation to normal form.
chronoNFIO :: NFData a => IO a -> IO (Chronograph a)
chronoNFIO = chronoIOBy rnf

-- | Add a 'Chronograph' to measure time of IO and evaluation with the
-- provided strategy.
chronoIOBy :: (a -> ()) -> IO a -> IO (Chronograph a)
chronoIOBy eval mx = do
    t0 <- getCurrentTime
    val  <- mx
    t1 <- eval val `seq` getCurrentTime
    let measure = t1 .-. t0
    return $ Chronograph {val, measure}

----------------------------------------------------------
-- utility functions

-- | print the measure to stdout and return the value
chronoPrint :: String -> Chronograph a -> IO a
chronoPrint x cr =
    val cr <$ putStrLn (x ++ ": " ++ humanTimeDiff (measure cr))

-- | write the measure to the ghc eventlog and return the value
chronoTraceEvent :: String -> Chronograph a -> a
chronoTraceEvent x cr =
    traceEvent (x ++ ": " ++ humanTimeDiff (measure cr)) $ val cr

-- | write the measure to the ghc eventlog and return the value in IO
chronoTraceEventIO :: String -> Chronograph a -> IO a
chronoTraceEventIO x cr =
    val cr <$ traceEventIO (x ++ ": " ++ humanTimeDiff (measure cr))
