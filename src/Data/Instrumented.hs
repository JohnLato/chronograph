{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Instrumented (
  Instrumented (..)
-- * instrumentBy pure stuff
, instrument
, instrumentNF
, instrumentBy
-- * instrumentBy IO stuff
, instrumentJustIO
, instrumentIO
, instrumentNFIO
, instrumentIOBy
) where

import Control.DeepSeq
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)

data Instrumented a = Instrumented
    { val :: a
    , measure :: NominalDiffTime
    } deriving (Show, Generic)

----------------------------------------------------------
-- instrumentBy pure functions

-- | Instrument the time to evaluate an expression to weak head normal form.
instrument :: a -> Instrumented a
instrument = instrumentBy (`seq` ())

-- | Instrument the time to evaluate an expression to normal form.
instrumentNF :: NFData a => a -> Instrumented a
instrumentNF = instrumentBy rnf

-- | Instrument the time to evaluate an expression with the specified
-- evaluation strategy.
instrumentBy :: (a -> ()) -> a -> Instrumented a
instrumentBy eval x =
    let measure = unsafePerformIO $ do
                    t0 <- getCurrentTime
                    t1 <- eval x `seq` getCurrentTime
                    return (t1 `diffUTCTime` t0)
    in Instrumented {val = measure `seq` x, measure }

----------------------------------------------------------
-- instrumentBy IO computations

-- | Instrument only the time necessary to perform the IO actions, with no
-- extra evaluations.
instrumentJustIO :: IO a -> IO (Instrumented a)
instrumentJustIO = instrumentIOBy (const ())

-- | Instrument the time to perform an IO action and evaluate to weak head
-- normal form.
instrumentIO :: IO a -> IO (Instrumented a)
instrumentIO = instrumentIOBy (`seq` ())

-- | Instrument the time to perform an IO action and evaluate to normal form.
instrumentNFIO :: NFData a => IO a -> IO (Instrumented a)
instrumentNFIO = instrumentIOBy rnf

-- | Instrument an IO action with the specified evaluation.
instrumentIOBy :: (a -> ()) -> IO a -> IO (Instrumented a)
instrumentIOBy eval mx = do
    t0 <- getCurrentTime
    val  <- mx
    t1 <- eval val `seq` getCurrentTime
    let measure = t1 `diffUTCTime` t0

    return $ Instrumented {val, measure}
