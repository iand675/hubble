module GitHub.Activity.Watching where

watchers :: Owner -> Repo -> m (Vector Watcher)

watchedRepositories :: Username -> m (Vector Repository)

userWatchedRepositories :: m (Vector Repository)

repositorySubscription :: Owner -> Repo -> m Subscription

setRepositorySubscription :: Owner -> Repo -> SubscriptionOptions -> m Subscription

deleteRepositorySubscription :: Owner -> Repo -> m ()

