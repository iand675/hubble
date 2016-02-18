module GitHub.Repositories.Releases where

getReleases
  :: GitHubM s m
  => Owner
  -> RepoName
  -> m (GitHubResponse (Vector Release))
getRepositoryReleases (Owner o) (RepoName r)
  = get (v3 json) ["repos", o, r, "releases"] []

getRelease
  :: GitHubM s m
  => Owner
  -> RepoName
  -> ReleaseId
  -> m (GitHubResponse Release)
getRelease (Owner o) (RepoName r) (ReleaseId rid)
  = get (v3 json) ["repos", o, r, "releases", rid] []

getLatestRelease
  :: GitHubM s m
  => Owner
  -> RepoName
  -> m (GitHubResponse Release)
getLatestRelease (Owner o) (RepoName r)
  = get (v3 json) ["repos", o, r, "releases", "latest"] []

getReleaseByTag
  :: GitHubM s m
  => Owner
  -> RepoName
  -> TagName
  -> m (GitHubResponse Release)
getReleaseByTag (Owner o) (RepoName r) (TagName t)
  = get (v3 json) ["repos", o, r, "releases", "tags", t] []

createRelease
  :: GitHubM s m
  => Owner
  -> RepoName
  -> NewRelease
  -> m (GitHubResponse Release)
createRelease (Owner o) (RepoName r)
  = post (v3 json) ["repos", o, r, "releases"] []

editRelease
  :: GitHubM s m
  => Owner
  -> RepoName
  -> ReleaseId
  -> UpdatedRelease
  -> m (GitHubResponse Release)
editRelease (Owner o) (RepoName r) (ReleaseId rid)
  = patch (v3 json) ["repos", o, r, "releases", rid] []

deleteRelease
  :: GitHubM s m
  => Owner
  -> RepoName
  -> ReleaseId
  -> m (GitHubResponse ())
deleteRelease (Owner o) (RepoName r) (ReleaseId rid)
  = delete (v3 json) ["repos", o, r, "releases", rid] []

getReleaseAssets
  :: GitHubM s m
  => Owner
  -> RepoName
  -> ReleaseId
  -> m (GitHubResponse (Vector Asset))
getReleaseAssets (Owner o) (RepoName r) (ReleaseId rid)
  = get (v3 json) ["repos", o, r, "releases", rid, "assets"] []

{-
TODO

uploadReleaseAsset
  :: GitHubM s m
  => Owner
  -> RepoName
  -> ReleaseId
  -> ...

getReleaseAsset
  :: GitHubM s m
  => Owner
  -> RepoName
  -> AssetId
  -> m ...
-}

editReleaseAsset
  :: GitHubM s m
  => Owner
  -> RepoName
  -> AssetId
  -> UpdatedAsset
  -> m (GitHubResponse Asset)
editReleaseAsset (Owner o) (RepoName r) (AssetId a)
  = patch (v3 json) ["repos", o, r, "releases", "assets", a] []

deleteReleaseAsset
  :: GitHubM s m
  => Owner
  -> Repo
  -> AssetId
  -> m (GitHubResponse ())
deleteReleaseAsset (Owner o) (RepoName r) (AssetId a)
  = delete (v3 json) ["repos", o, r, "releases", "assets", a] []
