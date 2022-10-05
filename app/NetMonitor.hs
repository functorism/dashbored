{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module NetMonitor where

import Control.Applicative (Applicative (liftA2))
import Data.Attoparsec.ByteString (Parser, parseOnly)
import Data.Attoparsec.ByteString.Char8 (char, decimal, skipSpace, takeTill)
import qualified Data.ByteString.Char8 as BS
import Data.Either (rights)
import Data.Map (Map, fromAscList)
import Data.Maybe (fromMaybe)
import Data.Semialign (padZipWith)
import Numeric (showFFloat)

type Interface = BS.ByteString
type Bytes = Int

readProcNetDev :: IO BS.ByteString
readProcNetDev = BS.readFile "/proc/net/dev"

data NetDev = NetDev
  { rx_bytes :: Int,
    rx_packets :: Int,
    rx_errs :: Int,
    rx_drop :: Int,
    rx_fifo :: Int,
    rx_frame :: Int,
    rx_compressed :: Int,
    rx_multicast:: Int,
    tx_bytes :: Int,
    tx_packets :: Int,
    tx_errs :: Int,
    tx_drop :: Int,
    tx_fifo :: Int,
    tx_colls :: Int,
    tx_carrier :: Int,
    tx_compressed :: Int
  }
  deriving (Eq, Ord, Show)

getNetDevs :: IO (Map Interface NetDev)
getNetDevs = parseNetDevs <$> readProcNetDev

parseNetDevs :: BS.ByteString -> Map Interface NetDev
parseNetDevs = fromAscList . rights . (parseOnly netLine <$>) . drop 2 . BS.lines

rate :: Fractional d => d -> Int -> Int -> d
rate dx prev next = fromIntegral (next - prev) / dx

rates :: Fractional d => d -> Map Interface Int -> Map Interface Int -> Map Interface d
rates = padZipWith . ((fromMaybe 0 .) .) . liftA2 . rate

rxRates :: Fractional d => d -> Map Interface NetDev -> Map Interface NetDev -> Map Interface d
rxRates dx prev next = rates dx (rx_bytes <$> prev) (rx_bytes <$> next)

txRates :: Fractional d => d -> Map Interface NetDev -> Map Interface NetDev -> Map Interface d
txRates dx prev next = rates dx (tx_bytes <$> prev) (tx_bytes <$> next)

showBytes :: (RealFloat n, Show n, Ord n, Fractional n) => n -> String
showBytes n | n < 1000 = show n <> " b"
showBytes n | n < 1_000_000 = showFFloat (Just 2) (n / 1000) " Kb"
showBytes n | n < 1_000_000_000 = showFFloat (Just 2) (n / 1_000_000) " Mb"
showBytes n = showFFloat (Just 2) (n / 1_000_000_000) " Gb"


-- | See: https://www.kernel.org/doc/Documentation/filesystems/proc.txt
-- | Interface line of /proc/net/dev has the following format:
-- | >     lo:  338622 2435 0 0 0 0 0 0 338622 2435 0 0 0 0 0 0
-- | > wlp58s0: 101990112 75165 0 0 0 0 0 890 2387882 24723 0 0 0 0 0 0
netLine :: Parser (Interface, NetDev)
netLine =
  (,)
    <$> (skipSpace *> takeTill (== ':') <* char ':')
    <*> ( NetDev
            <$> (skipSpace *> decimal)
            <*> (skipSpace *> decimal)
            <*> (skipSpace *> decimal)
            <*> (skipSpace *> decimal)
            <*> (skipSpace *> decimal)
            <*> (skipSpace *> decimal)
            <*> (skipSpace *> decimal)
            <*> (skipSpace *> decimal)
            <*> (skipSpace *> decimal)
            <*> (skipSpace *> decimal)
            <*> (skipSpace *> decimal)
            <*> (skipSpace *> decimal)
            <*> (skipSpace *> decimal)
            <*> (skipSpace *> decimal)
            <*> (skipSpace *> decimal)
            <*> (skipSpace *> decimal)
        )
