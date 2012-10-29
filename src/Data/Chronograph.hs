{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

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
) where

import Control.DeepSeq
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)

data Chronograph a = Chronograph
    { val :: a
    , measure :: NominalDiffTime
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
                    return (t1 `diffUTCTime` t0)
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
    let measure = t1 `diffUTCTime` t0

    return $ Chronograph {val, measure}
