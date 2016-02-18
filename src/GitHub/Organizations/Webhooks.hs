module GitHub.Organizations.Webhooks where

getHooks
  :: OrganizationName
  -> m (Page Hook)
getHooks (OrganizationName o)
  =

getHook
  :: OrganizationName
  -> HookId
  -> m Hook
getHooks (OrganizationName o)
  =

{-
createHook
  :: Org
  -> NewHook
  -> m Hook

editHook
  :: Org
  -> HookId
  -> UpdatedHook
  -> m Hook

pingHook
  :: Org
  -> HookId
  -> m Hook

deleteHook
  :: Org
  -> HookId
  -> m ()
-}
-- TODO support for receiving hooks

