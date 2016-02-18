module GitHub.Issues.Events where

repoEvents :: Owner -> Repo -> m (Page Events)

repoEvent :: Owner -> Repo -> EventId -> m Event


