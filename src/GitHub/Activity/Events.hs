module GitHub.Activity.Events where
import GitHub.Internal
import GitHub.Types

listPublicEvents
  :: GitHubM s m
  => m (GitHubResponse (Vector Event))
listPublicEvents
  = get (v3 json) ["events"] []

listRepositoryEvents
  :: GitHubM s m
  => Owner
  -> Repo
  -> m (GitHubResponse (Vector Event))
listRepositoryEvents
  = get (v3 json) ["repos", owner, repo, "events"] []

listRepositoryIssueEvents
  :: GitHubM s m
  => Owner
  -> Repo
  -> m (GitHubResponse (Vector Event))
listRepositoryIssueEvents
  = get (v3 json) ["repos", owner, repo, "issues", "events"]

listPublicRepositoryNetworkEvents
  :: GitHubM s m
  => Owner
  -> Repo
  -> m (GitHubResponse (Vector Event))
listPublicRepositoryNetworkEvents
  = get (v3 json) ["networks", owner, repo, "events"] []

listPublicOrganizationEvents
  :: GitHubM s m
  => Org
  -> m (GitHubResponse (Vector Event))
listPublicOrganizationEvents
  = get (v3 json) ["orgs", org, "events"] []

listUserReceivedEvents
  :: GitHubM s m
  => Username
  -> m (GitHubResponse (Vector Event))
listUserReceivedEvents
  = get (v3 json) ["users", username, "received_events"] []

listPublicUserReceivedEvents
  :: GitHubM s m
  => Username
  -> m (GitHubResponse (Vector Event))
listPublicUserReceivedEvents
  = get (v3 json) ["users", username, "received_events", "public"] []

listUserPerformedEvents
  :: GitHubM s m
  => Username
  -> m (GitHubResponse (Vector Event))
listUserPerformedEvents
  = get (v3 json) ["users", username, "events"] []

listPublicUserPerformedEvents
  :: GitHubM s m
  => Username
  -> m (GitHubResponse (Vector Event))
listPublicUserPerformedEvents
  = get (v3 json) ["users", username, "events", "public"] []

listOrganizationEvents
  :: GitHubM s m
  => Username
  -> Org
  -> m (GitHubResponse (Vector Event))
listOrganizationEvents
  = get (v3 json) ["users", username, "events", "orgs", org] []

