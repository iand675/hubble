module GitHub.Organizations.Teams where

teams :: Org
      -> m (Page Teams)

team :: TeamId
     -> m Team

createTeam :: Org
           -> NewTeam
           -> m Team

-- todo org permissions preview

editTeam :: TeamId
         -> UpdatedTeam
         -> m Team

deleteTeam :: TeamId
           -> m ()

data MemberRoleFilter = Member | Maintainer | AllMembers

teamMembers :: TeamId
            -> Maybe MemberRoleFilter
            -> m (Page Member)

teamMember :: TeamId
           -> Username
           -> m Member

teamMembership :: TeamId
               -> Username
               -> m Membership

data NewMembership = Member | Maintainer

addMembership :: TeamId
              -> Username
              -> NewMembership
              -> m Membership

removeMembership :: TeamId
                 -> Username
                 -> m ()

teamRepos :: TeamId
          -> m (Page Repository)

checkRepoManagedByTeam :: TeamId
                       -> Owner
                       -> Repo
                       -> m Bool

addTeamRepository :: TeamId
                  -> Org
                  -> Repo
                  -> TeamRepository
                  -> m ()

removeTeamRepository :: TeamId
                     -> Owner
                     -> Repo
                     -> m ()

userTeams :: m (Page Team)
