{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.Users where
import qualified Data.ByteString.Char8 as B
import Data.Vector (Vector)
import GitHub.Internal
import GitHub.Types

jsonize ''Plan
jsonize ''User
jsonize ''BasicUser

getUser
  :: GitHubM s m
  => Username
  -> m (GitHubResponse User)
getUser (Username u)
  = get (v3 json) ["users", u] []

getAuthenticatedUser
  :: GitHubM s m
  => m (GitHubResponse User)
getAuthenticatedUser
  = get (v3 json) ["user"] []

{-
updateAuthenticatedUser
-}

getAllUsers
  :: GitHubM s m
  => Maybe UserId
  -> m (GitHubResponse (Vector BasicUser))
getAllUsers mUserId
  = get (v3 json) ["users"] $ case mUserId of
      Nothing -> []
      Just (UserId u) -> [("since", Just $ B.pack $ show u)]

