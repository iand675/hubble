{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
module GitHub.Types where
import Control.Lens hiding (from)
import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Thyme.Clock
import Data.Thyme.Format.Aeson
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.Word (Word, Word64)

class RequestParams a where
  requestParams :: a -> [(ByteString, Maybe ByteString)]

-- $ Auth
newtype Token = Token ByteString
newtype Key = Key ByteString
newtype Secret = Secret ByteString
newtype Password = Password Text

data Auth
  = AuthToken Token
  | AuthKeyAndSecret Key Secret
  | BasicAuth Username Password

-- $ Owners

newtype OrganizationName = OrganizationName Text
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

orgName :: Text -> OrganizationName
orgName = OrganizationName

newtype Username = Username Text
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

username :: Text -> Username
username = Username

newtype Owner = Owner Text

class IsOwner a where
  toOwner :: a -> Owner

instance IsOwner OrganizationName where
  toOwner (OrganizationName t) = Owner t

instance IsOwner Username where
  toOwner (Username t) = Owner t 

-- $ Repositories

newtype RepoName = RepoName Text

-- $ Issue types

-- $$ Labels

newtype LabelName = LabelName Text
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

newtype HexColor = HexColor Text
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

data Label = Label
  { labelName  :: LabelName
  , labelUrl   :: Text
  , labelColor :: HexColor
  } deriving (Show)

data NewLabel = NewLabel
  { newLabelName  :: LabelName
  , newLabelColor :: HexColor
  } deriving (Show)

-- $$ Issues

{-
newtype IssueId = IssueId Int
  deriving (Show, Ord, Eq, ToJSON, FromJSON)

newtype IssueNumber = IssueNumber Int
  deriving (Show, Ord, Eq, ToJSON, FromJSON)

data IssueState
  = Open
  | Closed
  deriving (Show, Eq)

instance FromJSON IssueState where
  parseJSON (String x) = case x of
    "open"   -> pure Open
    "closed" -> pure Closed
    _ -> fail "Invalid issue state"
  parseJSON _ = fail "Invalid issue state"

data Issue = Issue
  { issueId_         :: IssueId
  -- TODO are these URLs useful?
  -- , issueUrl         :: Text
  -- , issueLabelsUrl   :: Text
  -- , issueCommentsUrl :: Text
  -- , issueEventsUrl   :: Text
  , issueHtmlUrl     :: Text
  , issueIssueNumber :: IssueNumber
  , issueState       :: IssueState
  , issueTitle       :: Text
  , issueBody        :: Text
  , issueUser        :: User
  , issueLabels      :: Vector Label
  , issueAssignee    :: Maybe User
  , issueMilestone   :: Maybe Milestone
  , issueLocked      :: Bool
  , issueComments    :: Int
  , issuePullRequest :: Maybe IssuePullRequest
  , issueClosedAt    :: Maybe UTCTime
  , issueCreatedAt   :: Maybe UTCTime
  , issueUpdatedAt   :: Maybe UTCTime
  }

-}

-- $ Organizations


newtype OrganizationId = OrganizationId Int
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

data Organization = Organization
  { organizationLogin       :: OrganizationName
  , organizationId_         :: OrganizationId
  , organizationUrl         :: Text
  , organizationAvatarUrl   :: Text
  , organizationDescription :: Maybe Text
  } deriving (Show)

-- $$ Webhooks

newtype WebhookId = WebhookId Int
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

data WebhookContentType
  = JsonWebhook
  | FormEncodedWebhook
  deriving (Show)

instance ToJSON WebhookContentType where
  toJSON JsonWebhook = String "application/json"
  toJSON FormEncodedWebhook = String "application/x-www-form-urlencoded"

instance FromJSON WebhookContentType where
  parseJSON (String str) = case str of
    "application/json" -> pure JsonWebhook
    "application/x-www-form-urlencoded" -> pure FormEncodedWebhook
    _ -> fail "Invalid webhook content type"
  parseJSON _ = fail "Invalid webhook content type"

data WebhookConfig = WebhookConfig
  { webhookConfigUrl         :: Text
  , webhookConfigContentType :: WebhookContentType
  } deriving (Show)

data Webhook = Webhooks
  { webhookId_       :: WebhookId
  , webhookName      :: Text
  -- TODO!!!!
  -- , webhookEvents    :: Vector EventType
  , webhookActive    :: Bool
  , webhookConfig    :: WebhookConfig
  , webhookUpdatedAt :: UTCTime
  , webhookCreatedAt :: UTCTime
  } deriving (Show)

-- $ Miscellaneous types

newtype TemplateName = TemplateName Text

data RateLimitResource = RateLimitResource
  { rateLimitResourceLimit     :: Word
  , rateLimitResourceRemaining :: Word
  , rateLimitResourceReset     :: Word64
  } deriving (Show)

instance FromJSON RateLimitResource where
  parseJSON (Object o) = RateLimitResource <$>
    o .: "limit" <*>
    o .: "remaining" <*>
    o .: "reset"
  parseJSON _ = fail "Invalid rate limit resource"

makeFields ''RateLimitResource

data RateLimitResources = RateLimitResources
  { rateLimitResourcesCore   :: RateLimitResource
  , rateLimitResourcesSearch :: RateLimitResource
  } deriving (Show)

instance FromJSON RateLimitResources where
  parseJSON (Object o) = RateLimitResources <$>
    o .: "core" <*>
    o .: "search"
  parseJSON _ = fail "Invalid rate limit resources"

makeFields ''RateLimitResources

newtype RateLimitInfo = RateLimitInfo
  { rateLimitResources :: RateLimitResources
  } deriving (Show)

instance FromJSON RateLimitInfo where
  parseJSON (Object o) = RateLimitInfo <$>
    o .: "resources"
  parseJSON _ = fail "Invalid rate limit info"

makeFields ''RateLimitInfo

-- $ Users

newtype UserId = UserId Int
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

data Plan = Plan
  { planName          :: Text
  , planSpace         :: Int
  , planPrivateRepos  :: Int
  , planCollaborators :: Int
  } deriving (Show)

makeFields ''Plan

data User = User
  { userLogin        :: Username
  , userId_          :: UserId
  , userAvatarUrl    :: Text
  , userGravatarId   :: Text
  -- , userUrl          :: Text
  , userHtmlUrl      :: Text
  -- , userFollowersUrl      :: Text
  -- , userFollowingUrl      :: Text
  -- , userGistsUrl          :: Text
  -- , userStarredUrl        :: Text
  -- , userSubscriptionsUrl  :: Text
  -- , userOrganizationsUrl  :: Text
  -- , userReposUrl          :: Text
  -- , userEventsUrl         :: Text
  -- , userReceivedEventsUrl :: Text
  , userType_             :: Text
  , userSiteAdmin         :: Bool
  , userName              :: Maybe Text
  , userCompany           :: Maybe Text
  , userBlog              :: Maybe Text
  , userLocation          :: Maybe Text
  , userEmail             :: Maybe Text
  , userHireable          :: Maybe Bool
  , userBio               :: Maybe Text
  , userPublicRepos       :: Int
  , userPublicGists       :: Int
  , userFollowers         :: Int
  , userFollowing         :: Int
  , userCreatedAt         :: UTCTime
  , userUpdatedAt         :: UTCTime
  , userTotalPrivateRepos :: Maybe Int
  , userOwnedPrivateRepos :: Maybe Int
  , userPrivateGists      :: Maybe Int
  , userDiskUsage         :: Maybe Int
  , userCollaborators     :: Maybe Int
  , userPlan              :: Maybe Plan
  } deriving (Show)

makeFields ''User

data BasicUser = BasicUser
  { basicUserLogin      :: Username
  , basicUserId_        :: UserId
  , basicUserGravatarId :: Text
  -- , basicUserUrl
  , basicUserHtmlUrl    :: Text
  , basicUserType_      :: Text
  , basicUserSiteAdmin  :: Bool
  } deriving (Show)

makeFields ''BasicUser

newtype EmailAddress = EmailAddress Text
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

data Email = Email
  { emailEmail    :: EmailAddress
  , emailPrimary  :: Bool
  , emailVerified :: Bool
  } deriving (Show)

makeFields ''Email

-- $ Repositories

newtype RepositoryId = RepositoryId Int

data RepositoryPermissions = RepositoryPermissions
  { repositoryPermissionsAdmin :: Bool
  , repositoryPermissionsPush  :: Bool
  , repositoryPermissionsPull  :: Bool
  }

newtype Ref = Ref Text
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

data Repository = Repository
  { repositoryId_              :: RepositoryId
  , repositoryOwner            :: User
  , repositoryName             :: Text
  , repositoryFullName         :: Text
  , repositoryDescription      :: Text
  , repositoryPrivate          :: Bool
  , repositoryFork             :: Bool
  , repositoryHtmlUrl          :: Text
  , repositoryGitUrl           :: Text
  , repositoryMirrorUrl        :: Text
  , repositorySshUrl           :: Text
  , repositorySvnUrl           :: Text
  , repositoryHomepage         :: Text
  , repositoryLanguage         :: Maybe Value -- TODO what is this
  , repositoryForksCount       :: Int
  , repositoryStargazersCount  :: Int
  , repositoryWatchersCount    :: Int
  , repositorySize             :: Int
  , repositoryDefaultBranch    :: Ref
  , repositoryOpenIssuesCount  :: Int
  , repositoryHasIssues        :: Bool
  , repositoryHasWiki          :: Bool
  , repositoryHasPages         :: Bool
  , repositoryHasDownloads     :: Bool
  , repositoryPushedAt         :: UTCTime
  , repositoryCreatedAt        :: UTCTime
  , repositoryUpdatedAt        :: UTCTime
  , repositoryPermissions      :: RepositoryPermissions
  , repositorySubscribersCount :: Int
  , repositoryOrganization     :: Maybe Organization
  , repositoryParent           :: Maybe Repository
  , repositorySource           :: Maybe Repository
  }

makeFields ''Repository

-- $
