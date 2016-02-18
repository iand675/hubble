{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.Issues.Labels where
import Data.Monoid
import Data.Vector (Vector)
import GitHub.Types
import GitHub.Internal

jsonize ''Label

getLabels :: GitHubM s m => Owner -> RepoName -> m (GitHubResponse (Vector Label))
getLabels (Owner o) (RepoName r)
  = get (v3 json) ["repos", o, r, "labels"] []

getLabel :: GitHubM s m => Owner -> RepoName -> LabelName -> m (GitHubResponse Label)
getLabel (Owner o) (RepoName r) (LabelName l)
  = get (v3 json) ["repos", o, r, "labels", l] []

{-

createLabel :: Owner -> Repo -> NewLabel -> m Label

updateLabel :: Owner -> Repo -> LabelName -> NewLabel -> m Label

deleteLabel :: Owner -> Repo -> LabelName -> m ()

issueLabels :: Owner -> Repo -> IssueId -> m (Page Label)

addIssueLabels :: Owner -> Repo -> f LabelName -> m (Vector Label)

removeIssueLabel :: Owner -> Repo -> IssueId -> LabelName -> m ()

replaceIssueLabels :: Owner -> Repo -> f LabelName -> m (Vector Label)

removeIssueLabels :: Owner -> Repo -> IssueId -> m ()

milestoneLabels :: Owner -> Repo -> MilestoneId -> m (Page Label)
-}
