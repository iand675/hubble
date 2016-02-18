{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.Organizations where
import qualified Data.ByteString.Char8 as B
import Data.Vector (Vector)
import GitHub.Internal
import GitHub.Types

jsonize ''Organization

getOrganizations
  :: GitHubM s m
  => m (GitHubResponse (Vector Organization))
getOrganizations
  = get (v3 json) ["user", "orgs"] []

getAllOrganizations
  :: GitHubM s m
  => Maybe OrganizationId
  -> m (GitHubResponse (Vector Organization))
getAllOrganizations mOrg
  = get (v3 json) ["organizations"] $ case mOrg of
      Nothing -> []
      Just (OrganizationId orgId) -> [("since", Just (B.pack $ show orgId))]

getUserOrganizations
  :: GitHubM s m
  => Username
  -> m (GitHubResponse (Vector Organization))
getUserOrganizations (Username u)
  = get (v3 json) ["users", u, "orgs"] []

getOrganization
  :: GitHubM s m
  => OrganizationName
  -> m (GitHubResponse Organization)
getOrganization (OrganizationName n)
  = get (v3 json) ["orgs", n] []

{-
editOrganizations
  :: OrgName
  -> UpdatedOrganization
  -> m Organization
-}
