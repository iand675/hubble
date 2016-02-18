module GitHub.Organizations.Members where

data MembersFilter = TwoFactorAuthDisabled | AllMembers
data RoleFilter = AllRoles | Admin | Member

members
  :: Org
  -> Maybe Filter
  -> Maybe Role
  -> m (Page Member)

-- TODO look into beta org permission api

checkMembership
  :: Org
  -> Username
  -> m Bool

removeMember
  :: Org
  -> Username
  -> m Bool

publicMembers
  :: m (Page Member)

checkPublicMembership
  :: Org
  -> Username
  -> m Bool

publicizeMembership
  :: Org
  -> Username
  -> m ()

concealMembership 
  :: Org
  -> Username
  -> m ()

membership
  :: Org
  -> Username
  -> m Membership

data Role = Admin | Member

setMembership
  :: Org
  -> Username
  -> Role
  -> m Membership

deleteMembership
  :: Org
  -> Username
  -> m ()

data MembershipState = Active | Pending

memberships
  :: Maybe MembershipState
  -> m (Page Membership)

membership
  :: Org
  -> m Membership

data EditMembership = SetActive

editMembership
  :: Org
  -> EditMembership
  -> m Membership

