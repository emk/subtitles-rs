module Definitions (AppEffects(), AppAff(), Time(Time), controls) where

import Prelude

import Data.Maybe (Maybe(..))
import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Console
import Unsafe.Coerce (unsafeCoerce)

import Halogen
import Halogen.HTML.Core (prop, propName, attrName)
import Halogen.HTML.Properties.Indexed (IProp(..), I())


-- The effects used by our application.
type AppEffects eff = HalogenEffects (console :: CONSOLE | eff)

-- The monad in which we'll run most of our computations.
type AppAff = Aff (AppEffects ())

-- A time in seconds, or fractions thereof.
newtype Time = Time Number

controls' :: forall i. Boolean -> Prop i
controls' = prop (propName "controls") (Just $ attrName "controls")

-- Video player controls.
controls :: forall r i. Boolean -> IProp (controls :: I | r) i
controls = unsafeCoerce controls'
