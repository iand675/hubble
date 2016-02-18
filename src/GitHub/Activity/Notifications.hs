module GitHub.Activity.Notifications where

listNotifications :: GitHubMonad m => m (Vector Notification)

listRepositoryNotifications :: GitHubMonad m => Owner -> Repo -> m (Vector Notification)

markNotificationsRead :: GitHubMonad m => Maybe UTCTime -> m ()

markRepositoryNotificationsRead :: GitHubMonad m => Maybe UTCTime -> m ()

-- TODO
-- notificationThread :: NotificationThreadId -> m 

-- markNotificationThreadRead

-- threadSubscription

-- setThreadSubscription

-- deleteThreadSubscription

