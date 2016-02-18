module GitHub.Webhooks where
import qualified Data.HashMap.Strict as H

eventList :: [(Text, EventName)]
eventList =
  [ ("*", WildcardEvent)
  , ("commit_comment", CommitCommentEvent)
  , ("create", CreateEvent)
  , ("delete", DeleteEvent)
  , ("deployment", DeploymentEvent)
  , ("deployment_status", DeploymentStatusEvent)
  , ("fork", ForkEvent)
  , ("gollum", GollumEvent)
  , ("issue_comment", IssueCommentEvent)
  , ("issues", IssuesEvent)
  , ("member", MemberEvent)
  , ("membership", MembershipEvent)
  , ("page_build", PageBuildEvent)
  , ("public", PublicEvent)
  , ("pull_request_review_comment", PullRequestReviewComment)
  , ("pull_request", PullRequestEvent)
  , ("push", PushEvent)
  , ("repository", RepositoryEvent)
  , ("release", ReleaseEvent)
  , ("status", StatusEvent)
  , ("team_add", TeamAddEvent)
  , ("watch", WatchEvent)
  , ("ping", PingEvent)
  ]

eventDictionary :: H.HashMap Text EventName
eventDictionary
  = H.fromList eventList

reverseEventDictionary :: H.HashMap EventName Text
reverseEventDictionary
  = H.fromList $ map (\(l, r) -> (r, l)) eventList

data EventName
  = WildcardHook
  | CommitCommentHook
  | CreateHook
  | DeleteHook
  | DeploymentHook
  | DeploymentStatusHook
  | ForkHook
  | GollumHook
  | IssueCommentHook
  | IssuesHook
  | MemberHook
  | MembershipHook
  | PageBuildHook
  | PublicHook
  | PullRequestReviewCommentHook
  | PullRequestHook
  | PushHook
  | RepositoryHook
  | ReleaseHook
  | StatusHook
  | TeamAddHook
  | WatchHook
  -- Special events
  | PingHook
  | OtherHook Text

class Payload a where
  payload :: EventName -> IO B.ByteString -> IO a

-- | This instance is special, since
-- it may be called multiple times.
-- See the requestBody function from WAI
-- for an intuition on how to use this instance.
instance Payload B.ByteString where
  payload _ = id

-- TODO internal
brConsume :: IO B.ByteString -> IO [B.ByteString]
brConsume brRead =
    go id
  where
    go front = do
        x <- brRead
        if B.null x
            then return $ front []
            else go (front . (x:))

instance Payload L.ByteString where
  payload _ = (fmap L.fromChunks) . brConsume

instance Payload (Either String Event) where
  payload e b = do
    lbs <- payload e b
    -- switch on event type

-- | TODO make sure body is retrievable for storage
data EventPayload a
  = EventPayload
      { eventPayloadEventName :: EventName
      , eventPayloadSignature :: Maybe Signature
      , eventPayloadDelivery  :: DeliveryId
      , eventPayloadContents  :: a
      }


data CommitCommentAction
  = CommitCommentCreated
  | CommitCommentDeleted

data CommitCommentEvent = CommitCommentEvent
  { commitCommentEventAction     :: CommitCommentAction    
  , commitCommentEventComment    :: Comment
  , commitCommentEventRepository :: Repository
  , commitCommentEventSender     :: User
  }


data CreateRefType
  = CreateRepositoryRef
  | CreateTagRef
  | CreateBranchRef

data CreateEvent = CreateEvent
  { createEventRefType      :: CreateRefType
  , createEventRef          :: Maybe Ref
  , createEventMasterBranch :: Branch
  , createEventDescription  :: Text
  , createEventPusherType   :: Text -- TODO type safety
  , createEventRepository   :: Repository
  , createEventSender       :: User
  }


data DeleteRefType
  = DeleteBranchRef
  | DeleteTagRef

data DeleteEvent = DeleteEvent
  { deleteEventRefType    :: DeleteRefType
  , deleteEventRef        :: Ref
  , deleteEventRepository :: Repository
  , deleteEventSender     :: User
  }


data DeploymentInfo a = DeploymentInfo
  { deploymentInfoId_         :: DeploymentId
  , deploymentInfoSha         :: SHA
  , deploymentInfoRef         :: Ref
  , deploymentInfoTask        :: Text -- TODO type safety
  , deploymentInfoPayload     :: a -- TODO, JSON object by default
  , deploymentInfoEnvironment :: Text
  , deploymentInfoDescription :: Maybe Text
  , deploymentInfoCreator     :: User
  , deploymentInfoCreatedAt   :: UTCTime
  , deploymentInfoUpdatedAt   :: UTCTime
  }

data DeploymentEvent a = DeploymentEvent
  { deploymentEventDeployment :: DeploymentInfo a
  , deploymentEventRepository :: Repository
  , deploymentEventSender     :: User
  }


data DeploymentState
  = DeploymentPending
  | DeploymentSuccess
  | DeploymentFailure
  | DeploymentError

data DeploymentStatusInfo = DeploymentStatusInfo
  { deploymentStatusInfoId          :: DeploymentStatusId
  , deploymentStatusInfoState       :: DeploymentState
  , deploymentStatusInfoCreator     :: User
  , deploymentStatusInfoDescription :: Maybe Text
  , deploymentStatusInfoTargetUrl   :: Maybe Text
  , deploymentStatusInfoCreatedAt   :: UTCTime
  , deploymentStatusInfoUpdatedAt   :: UTCTime
  }

data DeploymentStatusEvent a = DeploymentStatusEvent
  { deploymentStatusEventDeployment :: DeploymentInfo a
  , deploymentStatusEventStatus     :: DeploymentStatusInfo
  , deploymentStatusEventRepository :: Repository
  , deploymentStatusEventSender     :: User
  }

data ForkEvent = ForkEvent
  { forkEventForkee     :: Repository
  , forkEventRepository :: Repository
  , forkEventSender     :: User
  }


data PageAction
  = PageCreated
  | PageEdited

data PageEvent = PageEvent
  { pageEventPageName :: Text
  , pageEventTitle    :: Text
  , pageEventSummary  :: Maybe Text
  , pageEventAction   :: PageAction
  , pageEventSha      :: SHA
  , pageEventHtmlUrl  :: Text
  }


data GollumEvent = GollumEvent
  { gollumEventPages      :: Vector Page
  , gollumEventRepository :: Repository
  , gollumEventSender     :: User
  } deriving (Show)


data IssueCommentAction
  = IssueCommentCreated

data IssueCommentEvent = IssueCommentEvent
  { issueCommentEventAction     :: IssueCommentAction
  , issueCommentEventIssue      :: Issue
  , issueCommentEventComment    :: Comment
  , issueCommentEventRepository :: Repository
  , issueCommentEventSender     :: User
  }


data IssuesAction
  = IssueAssigned
  | IssueUnassigned
  | IssueLabeled
  | IssueUnlabeled
  | IssueOpened
  | IssueClosed
  | IssueReopened

data IssuesEvent = IssuesEvent
  { issuesEventAction     :: IssuesAction
  , issuesEventIssue      :: Issue
  , issuesEventAssignee   :: Maybe User
  , issuesEventLabel      :: Maybe Label
  , issuesEventRepository :: Repository
  , issuesEventSender     :: User
  }


data MemberAction
  = MemberAdded

data MemberEvent = MemberEvent
  { memberEventMember     :: User
  , memberEventAction     :: MemberAction
  , memberEventRepository :: Repository
  , memberEventSender     :: Sender
  }


data MembershipAction
  = MembershipAdded
  | MembershipRemoved

data MembershipScope
  = Team

data MembershipEvent = MembershipEvent
  { membershipEventAction       :: MembershipAction
  , membershipEventScope        :: MembershipScope
  , membershipEventMember       :: User
  , membershipEventTeam         :: Team
  , membershipEventSender       :: User
  , membershipEventOrganization :: Organization
  }


data PageBuildEvent = PageBuildEvent
  { pageBuildEventId         :: PageBuildId
  , pageBuildEventBuild      :: Build
  , pageBuildEventRepository :: Repository
  , pageBuildEventSender     :: User
  }

data PublicEvent = PublicEvent
  { publicEventRepository :: Repository
  , publicEventSender     :: User
  }


data PullRequestReviewCommentAction
  = PullRequestReviewCommentCreated

data PullRequestReviewCommentEvent = PullRequestReviewCommentEvent
  { pullRequestReviewCommentEventAction      :: PullRequestReviewCommentAction
  , pullRequestReviewCommentEventPullRequest :: PullRequest
  , pullRequestReviewCommentEventComment     :: Comment
  , pullRequestReviewCommentEventRepository  :: Repository
  , pullRequestReviewCommentEventSender      :: User
  }


data PullRequestAction
  = PullRequestAssigned
  | PullRequestUnassigned
  | PullRequestLabeled
  | PullRequestUnlabeled
  | PullRequestOpened
  | PullRequestClosed
  | PullRequestReopened
  | PullRequestSynchronize

data PullRequestEvent = PullRequestEvent
  { pullRequestEventAction      :: PullRequestAction
  , pullRequestEventNumber      :: PullRequestNumber
  , pullRequestEventPullRequest :: PullRequest
  , pullRequestEventRepository  :: Repository
  , pullRequestEventSender      :: User
  }


data PushEvent = PushEvent
  { pushEventRef        :: Text -- TODO this ref seems different from other uses in the API
  , pushEventBefore     :: SHA
  , pushEventAfter      :: SHA
  , pushEventCreated    :: Bool
  , pushEventDeleted    :: Bool
  , pushEventForced     :: Bool
  , pushEventBaseRef    :: Maybe Text -- TODO Ref?
  , pushEventCompare    :: Text
  , pushEventCommits    :: Vector Commit
  , pushEventHeadCommit :: Maybe Commit
  , pushEventRepository :: Repository
  , pushEventPusher     :: Pusher
  , pushEventSender     :: User
  }


data RepositoryAction
  = RepositoryCreated

data RepositoryEvent = RepositoryEvent
  { repositoryEventAction       :: RepositoryAction
  , repositoryEventRepository   :: Repository
  , repositoryEventOrganization :: Organization
  , repositoryEventSender       :: User
  }


data ReleaseAction
  = ReleasePublished

data ReleaseEvent = ReleaseEvent
  { releaseEventAction     :: ReleaseAction
  , releaseEventRelease    :: Release
  , releaseEventRepository :: Repository
  , releaseEventSender     :: User
  }


data StatusState
  = StatusPending
  | StatusSuccess
  | StatusFailure
  | StatusError

data StatusEvent = StatusEvent
  { statusEventId_         :: StatusId
  , statusEventSha         :: SHA
  , statusEventName        :: Text -- TODO what is this really
  , statusEventTargetUrl   :: Maybe Text
  , statusEventContext     :: Text -- TODO yeah what's this
  , statusEventDescription :: Maybe Text
  , statusEventState       :: StatusState
  , statusEventCommit      :: Commit
  , statusEventBranches    :: Vector Branch
  , statusEventCreatedAt   :: UTCTime
  , statusEventUpdatedAt   :: UTCTime
  , statusEventRepository  :: Repository
  , statusEventSender      :: User
  }


data TeamAddEvent = TeamAddEvent
  { teamAddEventTeam         :: Team
  , teamAddEventRepository   :: Repository
  , teamAddEventOrganization :: Organization
  , teamAddEventSender       :: User
  }


data WatchAction
  = WatchStarted

data WatchEvent = WatchEvent
  { watchEventAction       :: WatchAction
  , watchEventRepository   :: Repository
  , watchEventSender       :: User
  }


data PingEvent = PingEvent
  { pingEventZen    :: Text
  , pingEventHookId :: HookId
  , pingEventHook   :: Webhook
  }

data Other = Other Text Value

requestToEvent :: Payload a => Request -> IO (EventPayload a)

