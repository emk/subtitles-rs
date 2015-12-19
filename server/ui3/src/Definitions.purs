module Definitions (
  AppEffects(), Size(Size), Time(Time), Interval(Interval),
  onLoadedMetadata, targetSize, onTimeUpdate, targetCurrentTime,
  onPlay, onPause
  ) where

import Prelude
import Control.Monad.Eff.Console
import Data.Generic
import qualified React as R
import qualified React.DOM.Props as RP
import Unsafe.Coerce

-- Our standard effect types.
type AppEffects eff = (console :: CONSOLE | eff)

-- A width and a height.
data Size = Size Int Int

derive instance genericSize :: Generic Size
instance showSize :: Show Size where show = gShow

-- A time in seconds, or fractions thereof.
data Time = Time Number

derive instance genericTime :: Generic Time
instance showTime :: Show Time where show = gShow

-- An interval of time.
data Interval = Interval Time Time

derive instance genericInterval :: Generic Interval
instance showInterval :: Show Interval where show = gShow

-- An onLoadedMetadata handler, which isn't supplied by the React.DOM
-- library we're using.
onLoadedMetadata :: forall eff props state result.
  (R.Event -> R.EventHandlerContext eff props state result)
  -> RP.Props
onLoadedMetadata f = RP.unsafeMkProps "onLoadedMetadata" (R.handle f)

-- Get the video size for an onLoadedMetadata event.
targetSize :: R.Event -> Size
targetSize event = Size target.videoWidth target.videoHeight
  where target = (unsafeCoerce event).target

-- An onTimeUpdate handler.
onTimeUpdate :: forall eff props state result.
  (R.Event -> R.EventHandlerContext eff props state result)
  -> RP.Props
onTimeUpdate f = RP.unsafeMkProps "onTimeUpdate" (R.handle f)

-- Get the current playback time.
targetCurrentTime :: R.Event -> Time
targetCurrentTime event = Time target.currentTime
  where target = (unsafeCoerce event).target

onPlay :: forall eff props state result.
  (R.Event -> R.EventHandlerContext eff props state result)
  -> RP.Props
onPlay f = RP.unsafeMkProps "onPlay" (R.handle f)

onPause :: forall eff props state result.
  (R.Event -> R.EventHandlerContext eff props state result)
  -> RP.Props
onPause f = RP.unsafeMkProps "onPause" (R.handle f)
