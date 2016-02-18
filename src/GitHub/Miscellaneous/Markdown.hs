module GitHub.Miscellaneous.Markdown where

data RenderMode = Markdown | GFM Context

data Context = Context Owner Repo

render :: Text
       -> Maybe RenderMode
       -> m Text

renderRaw :: Text -> m Text

