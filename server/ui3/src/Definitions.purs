module Definitions (
  AppEffects(), Size(), Time(Time), Interval()
  ) where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console
import qualified DOM as DOM

-- Our standard effect types.
type AppEffects = (console :: CONSOLE, dom :: DOM.DOM)

-- A width and a height.
type Size = { width :: Int, height :: Int }

-- A time in seconds, or fractions thereof.
newtype Time = Time Number

instance showTime :: Show Time where
  show (Time t) = show t

-- An interval of time.
type Interval = { begin :: Time, end :: Time }
