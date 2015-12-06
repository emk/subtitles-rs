module Video (Model, init) where

-- Information about this video.  Will eventually also contain subtitles,
-- etc.
type alias Model = { url: String }

-- Create this model.
init : String -> Model
init url = Model url
