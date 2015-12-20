module VideoDOM (
  onLoadedMetadata, targetSize, onTimeUpdate, targetCurrentTime,
  onPlay, onPause, play, pause, setCurrentTime
  ) where

import Prelude

import Control.Monad.Eff (Eff())
import qualified DOM as DOM
import qualified React as R
import qualified React.DOM.Props as RP
import Unsafe.Coerce

import Definitions

-- An onLoadedMetadata handler, which isn't supplied by the React.DOM
-- library we're using.
onLoadedMetadata :: forall eff props state result.
  (R.Event -> R.EventHandlerContext eff props state result)
  -> RP.Props
onLoadedMetadata f = RP.unsafeMkProps "onLoadedMetadata" (R.handle f)

-- Get the video size for an onLoadedMetadata event.
targetSize :: R.Event -> Size
targetSize event = { width: target.videoWidth, height: target.videoHeight }
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

foreign import play :: forall eff. String -> Eff (dom :: DOM.DOM | eff) Unit

foreign import pause :: forall eff. String -> Eff (dom :: DOM.DOM | eff) Unit

foreign import setCurrentTime_ :: forall eff.
  String -> Number -> Eff (dom :: DOM.DOM | eff) Unit

setCurrentTime :: forall eff. String -> Time -> Eff (dom :: DOM.DOM | eff) Unit
setCurrentTime id (Time time) = setCurrentTime_ id time
