module GitHub.Search where

searchRepositories
  :: GitHubM s m
  => RepositorySearchQuery
  -> m (GitHubResponse RepositorySearchResults)

searchCode
:: GitHubM s m
=> CodeSearchQuery
-> m (GitHubResponse CodeSearchResults)

searchIssues
searchUsers

