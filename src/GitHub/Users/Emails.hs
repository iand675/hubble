{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
module GitHub.Users.Emails where
import qualified Data.ByteString.Char8 as B
import Data.Vector (Vector)
import GitHub.Internal
import GitHub.Types

jsonize ''Email

getUserEmails
  :: GitHubM s m
  => m (GitHubResponse (Vector Email))
getUserEmails
  = get (v3 json) ["user", "emails"] []

{-
addEmailAddresses
  :: GitHubM s m
  => Vector EmailAddress
  -> m (GitHubResponse (Vector Email))
addEmailAddresses
  = post (v3 json) ["user", "emails"] []

deleteEmailAddresses
  :: GitHubM s m
  => Vector EmailAddress
  -> m (GitHubResponse ())
deleteEmailAddresses
  = delete (v3 json) ["user", "emails"] []
-}
