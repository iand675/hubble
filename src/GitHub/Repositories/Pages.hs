module GitHub.Repositories.Pages where

getPagesInfo
  :: GitHubM s m
  => Owner
  -> RepoName
  -> m (GitHubResponse PagesInfo)
getPagesInfo (Owner o) (RepoName r)
  = get (v3 json) ["repos", o, r, "pages"] []

getPagesBuilds
  :: GitHubM s m
  => Owner
  -> RepoName
  -> m (GitHubResponse (Vector PagesBuild))
getPagesBuilds (Owner o) (RepoName r)
  = get (v3 json) ["repos", o, r, "pages", "builds"] []

getLatestPagesBuild
  :: GitHubM s m
  => Owner
  -> RepoName
  -> m (GitHubResponse (Maybe PagesBuild))
getLatestPagesBuild (Owner o) (RepoName r)
  = get (v3 json) ["repos", o, r, "pages", "builds", "latest"] []

