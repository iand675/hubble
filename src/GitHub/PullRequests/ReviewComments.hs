module GitHub.PullRequests.ReviewComments where

pullRequestComments :: Owner
                    -> Repo
                    -> PullRequestNumber
                    -> m (Page Comment)

data CommentSortBy = Created | Updated
data CommentSortDirection = Ascending | Descending

repositoryComments :: Owner
                   -> Repo
                   -> Maybe CommentSortBy
                   -> Maybe CommentSortDirection
                   -> Maybe UTCTime
                   -> m (Page Comment)

comment :: Owner
        -> Repo
        -> CommentId
        -> m Comment

createComment :: Owner
              -> Repo
              -> PullRequestNumber
              -> NewComment
              -> m Comment

editComment :: Owner
            -> Repo
            -> CommentId
            -> UpdatedComment
            -> m Comment

deleteComment :: Owner
              -> Repo
              -> CommentId
              -> m ()

