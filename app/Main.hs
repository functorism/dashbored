{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Bel (Bel (Bel, belAttrMap, belInit, belSubscriptions, belUpdate, belView), Dur (..), Sub (Every, FocusIn, FocusOut), Subscriptions, Update, View, belMain)
import BlockText (todToBlockText)
import Brick
import Brick.Widgets.Center (center, hCenter)
import Control.Monad (void)
import CpuInfo (CpuInfo, cpuLoadAvg, getCpuInfos, nextCpuInfos)
import Data.Map (Map, elems)
import Data.Time (LocalTime (localTimeOfDay), TimeOfDay (TimeOfDay), getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import GHC.Base (join)
import GHC.Float (float2Int)
import Graphics.Vty (Attr, blue, defAttr, green, red)
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
      cpuLoad = mempty,
      netMonitor = mempty,
      active = True
    }

update :: Update DashState DashEvent
update s NetMonitor = (\ifs -> s {netMonitor = take 100 $ ifs : netMonitor s}) <$> getNetDevs
update s CpuInfo = (\cpus -> s {cpuLoad = nextCpuInfos (cpuLoad s) cpus}) <$> getCpuInfos
update s Tick = (\t -> s {tod = t}) . localTimeOfDay <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)
update s (Focus b) = pure s {active = b}

subscriptions :: Subscriptions DashState DashEvent
subscriptions DashState {active} =
  [Every (Milliseconds 250) CpuInfo | active]
    <> [Every (Milliseconds 500) NetMonitor | active]
    <> [Every (Seconds 1) Tick | active]
    <> [FocusIn (Focus True), FocusOut (Focus False)]

viewCpuLoad :: Float -> Widget DashName
viewCpuLoad r = Widget Greedy Fixed $ do
  ctx <- getContext
  let len = availWidth ctx - 2
  let n = r * fromIntegral len
  render $ str $ join (replicate (float2Int n) "▰" <> replicate (len - float2Int n) "▱")

graphChar :: RealFrac n => n -> Char
graphChar n = "▁▂▃▄▅▆▇█" !! truncate (clamp 0 1 n * 7)

graphString :: (RealFrac n) => n -> [Int] -> String
graphString dx xs = (\n -> graphChar ((n - lo) / (hi - lo))) <$> reverse (take 15 rates)
  where
    rates = zipWith (rate dx) (drop 1 xs) xs
    lo = minimum rates
    hi = maximum rates

topTwo :: Monoid m => [m] -> (m, m)
topTwo (x1 : x2 : _) = (x2, x1)
topTwo _ = (mempty, mempty)

viewNetRate :: DashState -> Widget DashName
viewNetRate DashState {netMonitor} =
  padTopBottom 1 $
    hBox
      [ withAttr attrRx $
          hBox
            [ hCenter $ str $ "↓ " <> viewRate rxRates <> "/s",
              hCenter $ str (viewGraph rx_bytes)
            ],
        withAttr attrTx $
          hBox
            [ hCenter $ str (viewGraph tx_bytes),
              hCenter $ str $ "↑ " <> viewRate txRates <> "/s"
            ]
      ]
  where
    dx = 0.5 :: Float -- Second, see Subscription interval for NetMonitor event
    viewRate f = showBytes (sum (uncurry (f dx) (topTwo netMonitor)))
    viewGraph f = graphString dx (fromIntegral . sum . map f . elems <$> netMonitor)

viewClock :: TimeOfDay -> Widget DashName
viewClock tod = padLeftRight 2 (withAttr attrClock $ str (todToBlockText tod))

view :: View DashState DashName
view s@DashState {tod, cpuLoad} =
  center $
    viewClock tod
      <+> vBox (viewCpuLoad . cpuLoadAvg <$> cpuLoad)
      <=> viewNetRate s

attrTx :: AttrName
attrTx = attrName "tx"

attrRx :: AttrName
attrRx = attrName "rx"

attrClock :: AttrName
attrClock = attrName "clock"

attrs :: [(AttrName, Attr)]
attrs =
  [ (attrTx, fg green),
    (attrRx, fg red),
    (attrClock, fg blue)
  ]

app :: Bel DashState DashEvent DashName
app =
  Bel
    { belSubscriptions = subscriptions,
      belView = view,
      belUpdate = update,
      belInit = state,
      belAttrMap = attrMap defAttr attrs
    }

main :: IO ()
main = void $ belMain app
