module GitHub.PullRequests where

listPullRequests
  :: GitHubM s m
  => Owner
  -> Repo
  -> ListPullRequestsQuery
  -> m (GitHubResponse (Vector PullRequest))


