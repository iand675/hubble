module GitHub.Repositories.Merging where

performMerge
  :: GitHubM s m
  => Owner
  -> Repo
  -> MergeOptions
  -> m (GitHubResponse (Either MergeFailure (Maybe Commit)))
performMerge (Owner o) (RepoName r)
  = post (v3 json) ["repos", o, r, "merges"] []

