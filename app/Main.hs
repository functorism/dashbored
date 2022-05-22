{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import BlockText (todToBlockText)
import Brick
import Brick.Extra.Bel (Bel (Bel, belInit, belSubscriptions, belUpdate, belView), Dur (..), Sub (Every, FocusIn, FocusOut), Subscriptions, Update, View, belMain)
import Brick.Widgets.Center (center, hCenter)
import Control.Monad (void)
import CpuInfo (CpuInfo, cpuLoadAvg, getCpuInfos, nextCpuInfos)
import Data.Map (Map, elems)
import Data.Time (LocalTime (localTimeOfDay), TimeOfDay (TimeOfDay), getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import GHC.Base (join)
import GHC.Float (float2Int)
import NetMonitor (Interface, NetDev (rx_bytes, tx_bytes), getNetDevs, rate, rxRates, showBytes, txRates)

data DashState = DashState
  { tod :: TimeOfDay,
    cpuLoad :: [(CpuInfo, CpuInfo)],
    netMonitor :: [Map Interface NetDev],
    active :: Bool
  }
  deriving (Eq, Ord, Show)

data DashEvent = Tick | CpuInfo | NetMonitor | Focus Bool deriving (Eq, Ord, Show)

type DashName = ()

type DashApp = App DashState DashEvent DashName

state :: DashState
state =
  DashState
    { tod = TimeOfDay 0 0 0,
      cpuLoad = [],
      netMonitor = mempty,
      active = True
    }

viewCpuLoad :: Float -> Widget n
viewCpuLoad r = Widget Greedy Fixed $ do
  ctx <- getContext
  let len = availWidth ctx - 2
  let n = r * fromIntegral len
  render $ str $ join (replicate (float2Int n) "▰" <> replicate (len - float2Int n) "▱")

graphChar :: RealFrac n => n -> Char
graphChar n = "▁▂▃▄▅▆▇█" !! truncate (clamp 0 1 n * 7)

lastTwo :: Monoid b => [b] -> (b, b)
lastTwo (x1 : x2 : _) = (x2, x1)
lastTwo _ = (mempty, mempty)

graphString :: (RealFrac d) => d -> [Int] -> String
graphString dx xs = (\n -> graphChar ((n - lo) / (hi - lo))) <$> reverse rates
  where
    rates = zipWith (rate dx) (drop 1 xs) xs
    lo = minimum rates
    hi = maximum rates

viewNetRate :: DashState -> Widget DashName
viewNetRate DashState {netMonitor} =
  padTopBottom 1 $
    hBox
      [ hBox
          [ hCenter $ str $ "↓ " <> viewRate rxRates <> "/s",
            hCenter $ str (viewGraph rx_bytes)
          ],
        hBox
          [ hCenter $ str (viewGraph tx_bytes),
            hCenter $ str $ "↑ " <> viewRate txRates <> "/s"
          ]
      ]
  where
    dx = 0.1 :: Float -- See Subscription interval for NetMonitor event
    viewRate f = showBytes (sum (uncurry (f dx) (lastTwo netMonitor)))
    viewGraph f = graphString dx (fromIntegral . sum . map f . elems <$> netMonitor)

fillVertical :: Char -> Widget n
fillVertical c = Widget Greedy Fixed $ do
  ctx <- getContext
  let len = availWidth ctx - 2
  render $ str $ join (replicate len (pure c))

view :: View DashState DashName
view s@DashState {tod, cpuLoad} =
  center $
    padLeftRight 2 (str (todToBlockText tod))
      <+> vBox (viewCpuLoad . cpuLoadAvg <$> cpuLoad)
      <=> viewNetRate s

update :: Update DashState DashEvent
update s NetMonitor = (\ifs -> s {netMonitor = take 10 $ ifs : netMonitor s}) <$> getNetDevs
update s CpuInfo = (\cpus -> s {cpuLoad = nextCpuInfos (cpuLoad s) cpus}) <$> getCpuInfos
update s Tick = (\t -> s {tod = t}) . localTimeOfDay <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)
update s (Focus b) = pure s {active = b}

subscriptions :: Subscriptions DashState DashEvent
subscriptions DashState {active} =
  [Every (Milliseconds 100) CpuInfo | active]
    <> [Every (Milliseconds 100) NetMonitor | active]
    <> [Every (Seconds 1) Tick | active]
    <> [FocusIn (Focus True), FocusOut (Focus False)]

app :: Bel DashState DashEvent DashName
app =
  Bel
    { belSubscriptions = subscriptions,
      belView = view,
      belUpdate = update,
      belInit = state
    }

main :: IO ()
main = void $ belMain app
