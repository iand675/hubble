module GitHub.GitData.Commits where

getCommit :: Owner -> Repo -> SHA -> m Commit

createCommit :: Owner -> Repo -> NewCommit -> m Commit

