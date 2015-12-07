module Util (listFromMaybe) where

listFromMaybe : Maybe a -> List a
listFromMaybe maybeVal =
  case maybeVal of
    Just val -> [val]
    Nothing -> []
