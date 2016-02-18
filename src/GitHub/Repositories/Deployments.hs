module GitHub.Repositories.Deployments where

listDeployments
  :: GitHubM s m
  => Owner
  -> RepoName
  -> DeploymentFilterOptions
  -> m (GitHubResponse (Vector Deployment))
listDeployments (Owner o) (RepoName r) filterOpts
  = get (v3 json) ["repos", o, r, "deployments"] opts
  where
    -- TODO
    opts = undefined

createDeployment
  :: GitHubM s m
  => Owner
  -> RepoName
  -> NewDeployment
  -> m (GitHubResponse Deployment)
createDeployment (Owner o) (RepoName r)
  = post (v3 json) ["repos", o, r, "deployments"] []

listDeploymentStatuses
  :: GitHubM s m
  => Owner
  -> RepoName
  -> DeploymentId
  -> m (GitHubResponse (Vector DeploymentStatus))
listDeploymentStatuses (Owner o) (RepoName r) (DeploymentId d)
  = get (v3 json) ["repos", o, r, "deployments", pack $ show d] []

createDeploymentStatus
  :: GitHubM s m
  => Owner
  -> Repo
  -> DeploymentId
  -> NewStatus
  -> m (GitHubResponse DeploymentStatus)
createDeploymentStatus (Owner o) (RepoName r) (DeploymentId d)
  = post (v3 json) ["repos", o, r, "deployments", pack $ show d] []

