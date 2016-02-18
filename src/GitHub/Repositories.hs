module GitHub.Repositories where

data RepositoryVisibility
  = All
  | Public
  | Private

data Affiliation
  = Owner
  | Collaborator
  | OrganizationMember

data RepositoryType
  = AllRepositories
  | OwnerRepositories
  | PublicRepositories
  | PrivateRepositories
  | MemberRepositories

data RepositorySortBy
  = Created
  | Updated
  | Pushed
  | FullName

data RepositorySortDirection
  = Ascending
  | Descending

data RepositoriesRequest = RepositoriesRequest
  { repositoriesRequestType_         :: Maybe RepositoryType
  , repositoriesRequestSortBy        :: Maybe RepositorySortBy
  , repositoriesRequestSortDirection :: Maybe RepositorySortDirection
  } deriving (Show, Generic)

mkRequestParams ''RepositoriesRequest

repositories :: Maybe RepositoryVisibility
             -> f Affiliation
             -> RepositoriesRequest
             -> m (Page Repository)

data UserRepositoryType
  = All
  | Owner
  | Member

userRepositories :: Maybe UserRepositoryType
                 -> Maybe RepositorySortBy
                 -> Maybe RepositorySortDirection
                 -> m (Page Repository)

data OrganizationRepoType
  = AllOrganizationRepos
  | PublicOrganizationRepos
  | PrivateOrganizationRepos
  | ForkedByOrganizationRepos

organizationRepositories
  :: Org
  -> Maybe OrganizationRepoType
  -> m (Page Repository)

publicRepositories
  :: Maybe RepositoryId -- ^ since
  -> m (Page Repository)

createUserRepository
  :: Username
  -> NewUserRepository
  -> m Repository

createOrgRepository
  :: Org
  -> NewOrgRepository
  -> m Repository

repository
  :: Owner
  -> RepoName
  -> m Repository

editRepository
  :: Owner
  -> RepoName
  -> UpdatedRepository
  -> m Repository

contributors
  :: Owner
  -> RepoName
  -> Maybe Bool -- ^ include anonymous contributors
  -> m (Page Contributor)

languages
  :: Owner
  -> RepoName
  -> m (HashMap Text Int)

repositoryTeams
  :: Owner
  -> RepoName
  -> m (Page Team)

tags
  :: Owner
  -> RepoName
  -> m (Page TagInfo)

branches
  :: Owner
  -> RepoName
  -> Maybe Bool -- ^ filter by protected branches
  -> m (Page BranchInfo)

branch
  :: Owner
  -> RepoName
  -> BranchName
  -> m BranchInfo

setBranchProtection
  :: Owner
  -> RepoName
  -> BranchName
  -> BranchProtectionSettings
  -> m ()

deleteRepository
  :: Owner
  -> RepoName
  -> m ()

