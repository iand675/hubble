module GitHub.Repositories.DeployKeys where

listDeployKeys
  :: GitHubM s m
  => Owner
  -> RepoName
  -> m (GitHubResponse (Vector DeployKey))
listDeployKeys (Owner o) (RepoName r)
  = get (v3 json) ["repos", o, r, "keys"] []

getDeployKey
  :: GitHubM s m
  => Owner
  -> RepoName
  -> DeployKeyId
  -> m (GitHubResponse DeployKey)
getDeployKey (Owner o) (RepoName r) (DeployKeyId d)
  = get (v3 json) ["repos", o, r, "keys", pack $ show d] []

addDeployKey
  :: GitHubM s m
  => Owner
  -> RepoName
  -> NewDeployKey
  -> m (GitHubResponse DeployKey)
addDeployKey (Owner o) (RepoName r)
  = post (v3 json) ["repos", o, r, "keys"] []

removeDeployKey
  :: GitHubM s m
  => Owner
  -> RepoName
  -> DeployKeyId
  -> m (GitHubResponse ())
removeDeployKey
  = delete (v3 json) ["repos", o, r, "keys", pack $ show d] []

