module Main where

import Prelude

import qualified Data.Maybe.Unsafe as Unsafe
import Data.Nullable (Nullable(), toMaybe)
import Control.Monad.Eff

import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.ParentNode as DOM
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T

--import Definitions
--import VideoPlayer

-- Our application's state.
data State = State

-- The actions we can perform on our application.
data Action = DoNothing

initialState :: State
initialState = State

hello :: forall eff props. T.Spec eff State props Action
hello = T.simpleSpec performAction render
  where

  --render :: T.Render State props Action
  render dispatch _ _state _ =
    [ R.p [ RP.key "hello" ] [ R.text "Hello, world!" ] ]

  --performAction :: T.PerformAction eff State props Action
  performAction DoNothing _ state k = pure unit

unsafeFromNullable :: forall a. Nullable a -> a
unsafeFromNullable = Unsafe.fromJust <<< toMaybe

main :: Eff (dom :: DOM.DOM) Unit
main = void do
  let component = T.createClass hello initialState
  document <- DOM.window >>= DOM.document
  let root = DOM.htmlDocumentToParentNode document
  container <- unsafeFromNullable <$> DOM.querySelector "#container" root
  R.render (R.createFactory component {}) container
