{-# LANGUAGE OverloadedStrings #-}
module GitHub.Miscellaneous.Emoji where
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import GitHub.Internal hiding (Text)

getEmojis :: (GitHubM s m) => m (GitHubResponse (HashMap Text Text))
getEmojis = get (v3 json) ["emojis"] []

