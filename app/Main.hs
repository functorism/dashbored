{-# LANGUAGE NamedFieldPuns #-}

module Main where

import BlockText (todToBlockText)
import Brick
import Brick.Extra.Bel (Bel (Bel, belInit, belSubscriptions, belUpdate, belView), Dur (..), Sub (Every, FocusIn, FocusOut), Subscriptions, Update, View, belMain)
import Brick.Widgets.Center (center)
import Control.Monad (void)
import CpuInfo (CpuInfo, cpuLoadAvg, getCpuInfos, nextCpuInfos)
import Data.Time (LocalTime (localTimeOfDay), TimeOfDay (TimeOfDay), getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import GHC.Base (join)
import GHC.Float (float2Int)

data DashState = DashState
  { tod :: TimeOfDay,
    cpuLoad :: [(CpuInfo, CpuInfo)],
    active :: Bool
  }
  deriving (Eq, Ord, Show)

data DashEvent = Tick | CpuInfo | Focus Bool deriving (Eq, Ord, Show)

type DashName = ()

type DashApp = App DashState DashEvent DashName

state :: DashState
state =
  DashState
    { tod = TimeOfDay 0 0 0,
      cpuLoad = [],
      active = True
    }

viewCpuLoad :: Float -> Widget n
viewCpuLoad r = Widget Greedy Fixed $ do
  ctx <- getContext
  let len = availWidth ctx - 2
  let n = r * fromIntegral len
  render $ str $ join (replicate (float2Int n) "▰" <> replicate (len - float2Int n) "▱")

fillVertical :: Char -> Widget n
fillVertical c = Widget Greedy Fixed $ do
  ctx <- getContext
  let len = availWidth ctx - 2
  render $ str $ join (replicate len (pure c))

view :: View DashState DashName
view DashState {tod, cpuLoad} =
  center $
    str (todToBlockText tod)
      <=> fillVertical ' '
      <=> fillVertical ' '
      <=> vBox (viewCpuLoad . cpuLoadAvg <$> cpuLoad)

update :: Update DashState DashEvent
update s CpuInfo = (\cpus -> s {cpuLoad = nextCpuInfos (cpuLoad s) cpus}) <$> getCpuInfos
update s Tick = (\t -> s {tod = t}) . localTimeOfDay <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)
update s (Focus b) = pure s {active = b}

subscriptions :: Subscriptions DashState DashEvent
subscriptions DashState {active} =
  [Every (Milliseconds 250) CpuInfo | active]
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
