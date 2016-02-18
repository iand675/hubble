module GitHub.Repositories.Webhooks where

listHooks
  :: GitHubM s m
  => Owner
  -> RepoName
  -> m (GitHubResponse (Vector Webhook))
listHooks (Owner o) (RepoName r)
  = get (v3 json) ["repos", o, r, "hooks"] []

getHook
  :: GitHubM s m
  => Owner
  -> RepoName
  -> WebhookId
  -> m (GitHubResponse Webhook)
getHook (Owner o) (RepoName r) (WebhookId w)
  = get (v3 json) ["repos", o, r, "hooks", w] []

createHook
  :: GitHubM s m
  => Owner
  -> RepoName
  -> NewWebhook
  -> m (GitHubResponse Webhook)
createHook (Owner o) (RepoName r)
  = post (v3 json) ["repos", o, r, "hooks"] []

editHook
  :: GitHubM s m
  => Owner
  -> RepoName
  -> WebhookId
  -> UpdatedWebhook
  -> m (GitHubResponse Webhook)
editHook (Owner o) (RepoName r) (WebhookId w)
  = patch (v3 json) ["repos", o, r, "hooks", w] []

testPushHook
  :: GitHubM s m
  => Owner
  -> RepoName
  -> WebhookId
  -> m (GitHubResponse ())
testPushHook
  = post (v3 noContent) ["repos", o, r, "hooks", w, "tests"] [] ()

pingHook
  :: GitHubM s m
  => Owner
  -> RepoName
  -> WebhookId
  -> m (GitHubResponse ())
pingHook (Owner o) (RepoName r) (WebhookId w)
  = post (v3 noContent) ["repos", o, r, "hooks", w, "pings"] ()

deleteHook
  :: GitHubM s m
  => Owner
  -> RepoName
  -> WebhookId
  -> m (GitHubResponse ())
deleteHook (Owner o) (RepoName r) (WebhookId w)
  = delete (v3 noContent) ["repos", o, r, "hooks", w] []


