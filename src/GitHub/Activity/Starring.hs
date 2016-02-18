module GitHub.Activity.Starring where

listStargazers :: GitHubMonad m => Owner -> Repo -> m (Vector Stargazer)

listStarredRepositories :: GitHubMonad m => Username -> m (Vector Repository)

-- TODO: custom star media type

checkRepoIsStarred :: GitHubMonad m => Username -> Repo -> m Bool

starRepository :: GitHubMonad m => Username -> Repo -> m ()

unstarRepository :: GitHubMonad m => Username -> Repo -> m ()

