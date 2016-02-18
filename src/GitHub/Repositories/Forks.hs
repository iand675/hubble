module GitHub.Repositories.Forks where

data ForkSortBy
  = ForkSortByNewest
  | ForkSortByOldest
  | ForkSortByStargazers

sortStr :: ForkSortBy -> Text
sortStr s = case s of
  ForkSortByNewest -> "newest"
  ForkSortByOldest -> "oldest"
  ForkSortByStargazers -> "stargazers"

listForks
  :: GitHubM s m
  => Owner
  -> RepoName
  -> Maybe ForkSortBy
  -> m (GitHubResponse (Vector Repository))
listForks (Owner o) (RepoName r) msort
  = get (v3 json) ["repos", o, r, "forks"] $
      opt sortStr ("sort", msort) []
 
createFork
  :: GitHubM s m
  => Owner
  -> RepoName
  -> NewFork
  -> m (GitHubResponse Repository)
createFork (Owner o) (RepoName r) 
  = post (v3 json) ["repos", o, r, "forks"] []

