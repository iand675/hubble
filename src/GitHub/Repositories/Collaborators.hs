module GitHub.Repositories.Collaborators where

collaborators
  :: Owner
  -> Repo
  -> m (Page Collaborator)

checkCollaborator
  :: Owner
  -> Repo
  -> Username
  -> m Bool

data CollaboratorPermissions
  = Pull
  | Push
  | Admin

addCollaborator
  :: Owner
  -> Repo
  -> Username
  -> Maybe CollaboratorPermissions
  -> m ()

removeCollaborator
  :: Owner
  -> Repo
  -> Username
  -> m ()
