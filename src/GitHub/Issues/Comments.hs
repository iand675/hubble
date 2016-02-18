module GitHub.Issues.Comments where

issueComments :: Owner
              -> Repo
              -> IssueId
              -> Maybe UTCTime -- ^ since
              -> m (Vector Comment)


data RepoCommentSortBy = Created | Updated
data RepoCommentSortDirection = Ascending | Descending

repositoryComments :: Owner
                   -> Repo
                   -> Maybe RepoCommentSortBy -- ^ sort
                   -> Maybe RepoCommentSortDirection -- ^ direction
                   -> Maybe UTCTime -- ^ since
                   -> m (Vector Comment)

comment :: Owner -> Repo -> CommentId -> m Comment

createComment :: Owner -> Repo -> IssueId -> NewComment -> m Comment

editComment :: Owner -> Repo -> CommentId -> NewComment -> m Comment

deleteComment :: Owner -> Repo -> CommentId -> m ()

