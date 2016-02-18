module GitHub.Organizations.Migrations where

startMigration :: Org
               -> NewMigration
               -> m Migration

migrations :: Org
           -> m (Page Migration)

migration :: Org
          -> MigrationId
          -> m Migration

downloadMigration :: Org
                  -> MigrationId
                  -> m (Producer ByteString)

deleteMigration :: Org
                -> MigrationId
                -> m ()

unlockRepository :: Org
                 -> MigrationId
                 -> RepoName
                 -> m ()
