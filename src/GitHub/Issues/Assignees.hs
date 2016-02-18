module GitHub.Issues.Assignees where

assignees :: Owner -> Repo -> m (Vector Assignee)

checkAssignee :: Owner -> Repo -> Assignee -> m Bool
