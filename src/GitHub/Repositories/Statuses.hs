module GitHub.Repositories.Statuses where

createStatus
  :: GitHubM s m
  => Owner
  -> RepoName
  -> SHA
  -> NewStatus
  -> m (GitHubResponse Status)
createStatus (Owner o) (RepoName r) (SHA s)
  = post (v3 json) ["repos", o, r, "statuses", s] []

listRefStatuses
  :: GitHubM s m
  => Owner
  -> RepoName
  -> Ref
  -> m (GitHubResponse (Vector Status))
listRefStatuses (Owner o) (RepoName r) (Ref r)
  = get (v3 json) ["repos", o, r, "commits", r, "statuses"] []

getCombinedRefStatus 
  :: GitHubM s m
  => Owner
  -> RepoName
  -> Ref
  -> m (GitHubResponse CombinedStatus)
getCombinedRefStatus (Owner o) (RepoName r) (Ref r)
  = get (v3 json) ["repos", o, r, "commits", r, "statuses"] []

