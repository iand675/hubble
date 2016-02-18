module GitHub.GitData.References where

reference :: Owner -> Repo -> Ref -> m RefInfo

allReferences :: Owner -> Repo -> m (Vector RefInfo)

createReference :: Owner -> Repo -> NewRef -> m RefInfo

updateReference :: Owner -> Repo -> Ref -> RefUpdate -> m RefInfo

deleteReference :: Owner -> Repo -> Ref -> m ()
