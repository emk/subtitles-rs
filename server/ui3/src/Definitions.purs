module Definitions (
  AppEffects(), Size(Size), Time(Time), Interval(Interval),
  onLoadedMetadata, targetSize
  ) where

import Prelude
import Control.Monad.Eff (Eff())
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
newtype Time = Time Number

-- An interval of time.
data Interval = Interval Time Time

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
