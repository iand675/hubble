module GitHub.Issues where


data IssuesFilter
  = AssignedIssues
  | CreatedIssues
  | MentionedIssues
  | SubscribedIssues
  | AllIssues

data IssuesState
  = OpenIssues
  | ClosedIssues
  | AllIssueStates

data IssuesSort
  = ByCreated
  | ByUpdated
  | ByComments

issues
  :: Maybe IssuesFilter
  -> Maybe IssuesState
  -> f LabelName
  -> Maybe IssuesSort
  -> Maybe SortDirection
  -> Maybe UTCTime
  -> m (Page Issue)

userIssues
  :: Maybe IssuesFilter
  -> Maybe IssuesState
  -> f LabelName
  -> Maybe IssuesSort
  -> Maybe SortDirection
  -> Maybe UTCTime
  -> m (Page Issue)

orgIssues
  :: Org
  -> Maybe IssuesFilter
  -> Maybe IssuesState
  -> f LabelName
  -> Maybe IssuesSort
  -> Maybe SortDirection
  -> Maybe UTCTime
  -> m (Page Issue)

repoIssues
  :: Owner
  -> Repo
  -> Maybe MilestoneNumber
  -> Maybe IssuesState
  -> Maybe Username -- ^ assignee
  -> Maybe Username -- ^ creator
  -> Maybe Username -- ^ mentioned
  -> f LabelName
  -> Maybe IssuesSort
  -> Maybe SortDirection
  -> Maybe UTCTime
  -> m (Page Issue)

issue
  :: Owner
  -> Repo
  -> IssueNumber
  -> m Issue

createIssue
  :: Owner
  -> Repo
  -> NewIssue
  -> m Issue

editIssue
  :: Owner
  -> Repo
  -> IssueNumber
  -> UpdatedIssue
  -> m Issue

