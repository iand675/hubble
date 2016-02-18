module GitHub.Activity.Feeds where

listFeeds
  :: GitHubM s m
  => m (GitHubResponse Feeds)
listFeeds
  = get (v3 json) ["feeds"] []

