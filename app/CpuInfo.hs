{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CpuInfo where

import Control.Applicative (Applicative (liftA2))
import Data.Align (Semialign, padZipWith)
import Data.Attoparsec.ByteString (Parser, parseOnly, string)
import Data.Attoparsec.ByteString.Char8 (decimal, space)
import Data.ByteString as BS (ByteString, isPrefixOf, readFile)
import qualified Data.ByteString.Char8 as BS
import Data.Either (rights)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

readProcStat :: IO BS.ByteString
readProcStat = BS.readFile "/proc/stat"

data CpuInfo' a = CpuInfo
  { user :: a,
    nice :: a,
    system :: a,
    idle :: a,
    iowait :: a,
    irq :: a,
    softirq :: a,
    steal :: a,
    guest :: a,
    guest_nice :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Generic)

type CpuInfo = CpuInfo' Int

emptyCpuInfo :: CpuInfo
emptyCpuInfo = CpuInfo 0 0 0 0 0 0 0 0 0 0

getCpuInfos :: IO [CpuInfo]
getCpuInfos = parseCpuInfos <$> readProcStat

-- | cpu portion of /proc/stat files has the following format:
-- | > cpu  2763269 5775 816179 59225304 95792 0 39372 0 0 0
-- | > cpu0 664336 1350 212556 14827184 23709 0 2960 0 0 0
-- | > cpu1 685984 1446 207599 14760952 23961 0 31908 0 0 0
-- | > cpu2 712794 1473 199045 14808505 24212 0 3878 0 0 0
-- | > cpu3 700153 1504 196977 14828662 23909 0 624 0 0 0
parseCpuInfos :: BS.ByteString -> [CpuInfo]
parseCpuInfos = rights . (parseOnly cpuLine <$>) . takeWhile (BS.isPrefixOf "cpu") . drop 1 . BS.lines

cpuLoadRatio :: CpuInfo -> (Int, Int)
cpuLoadRatio cpu@CpuInfo {idle, iowait} = (sum cpu, idle + iowait)

nextCpuInfo :: (CpuInfo, CpuInfo) -> CpuInfo -> (CpuInfo, CpuInfo)
nextCpuInfo (_, prev) next = (prev, next)

padNextCpuInfo :: Maybe (CpuInfo, CpuInfo) -> Maybe CpuInfo -> (CpuInfo, CpuInfo)
padNextCpuInfo prevs next = fromMaybe (emptyCpuInfo, emptyCpuInfo) (liftA2 nextCpuInfo prevs next)

nextCpuInfos :: Semialign f => f (CpuInfo, CpuInfo) -> f CpuInfo -> f (CpuInfo, CpuInfo)
nextCpuInfos = padZipWith padNextCpuInfo

cpuLoadAvg :: (CpuInfo, CpuInfo) -> Float
cpuLoadAvg (prev, next) = fromIntegral utilized / fromIntegral totalDelta
  where
    utilized = totalDelta - idleDelta
    totalDelta = nextTotal - prevTotal
    idleDelta = nextIdle - prevIdle
    (prevTotal, prevIdle) = cpuLoadRatio prev
    (nextTotal, nextIdle) = cpuLoadRatio next

cpuLine :: Parser CpuInfo
cpuLine =
  string "cpu" >> (decimal :: Parser Int)
    >> CpuInfo
      <$> (space >> decimal)
      <*> (space >> decimal)
      <*> (space >> decimal)
      <*> (space >> decimal)
      <*> (space >> decimal)
      <*> (space >> decimal)
      <*> (space >> decimal)
      <*> (space >> decimal)
      <*> (space >> decimal)
      <*> (space >> decimal)
