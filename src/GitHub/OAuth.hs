{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub.OAuth where
import Control.Monad (join)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Hashable as H
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import GHC.Generics
import Network.Wai
import Network.HTTP.Types.Header
import Network.HTTP.Types.QueryLike
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI

import GitHub.Types (Key(..))

data Scope
  = User
  | UserEmail
  | UserFollow
  | PublicRepo
  | Repo
  | RepoDeployment
  | RepoStatus
  | DeleteRepo
  | Notifications
  | Gist
  | ReadRepoHook
  | WriteRepoHook
  | AdminRepoHook
  | AdminOrgHook
  | ReadOrg
  | WriteOrg
  | AdminOrg
  | ReadPublicKey
  | WritePublicKey
  | AdminPublicKey
  | OtherScope ByteString
  deriving (Show, Eq, Generic)

instance H.Hashable Scope

allScopes :: [Scope]
allScopes =
  [ User
  , UserEmail
  , UserFollow
  , PublicRepo
  , Repo
  , RepoDeployment
  , RepoStatus
  , DeleteRepo
  , Notifications
  , Gist
  , ReadRepoHook
  , WriteRepoHook
  , AdminRepoHook
  , AdminOrgHook
  , ReadOrg
  , WriteOrg
  , AdminOrg
  , ReadPublicKey
  , WritePublicKey
  , AdminPublicKey
  ]

scopesMap :: HM.HashMap ByteString Scope
scopesMap = HM.fromList $ map (\s -> (scopeRep s, s)) allScopes

scopeRep :: Scope -> ByteString
scopeRep s = case s of
  User           -> "user"
  UserEmail      -> "user:email"
  UserFollow     -> "user:follow"
  PublicRepo     -> "public_repo"
  Repo           -> "repo"
  RepoDeployment -> "repo_deployment"
  RepoStatus     -> "repo:status"
  DeleteRepo     -> "delete_repo"
  Notifications  -> "notifications"
  Gist           -> "gist"
  ReadRepoHook   -> "read:repo_hook"
  WriteRepoHook  -> "write:repo_hook"
  AdminRepoHook  -> "admin:repo_hook"
  AdminOrgHook   -> "admin:org_hook"
  ReadOrg        -> "read:org"
  WriteOrg       -> "write:org"
  AdminOrg       -> "admin:org"
  ReadPublicKey  -> "read:public_key"
  WritePublicKey -> "write:public_key"
  AdminPublicKey -> "admin:public_key"
  (OtherScope t) -> t

scope :: ByteString -> Scope
scope t = fromMaybe (OtherScope t) $ HM.lookup t scopesMap

newtype Nonce = Nonce ByteString

(?:) :: Maybe a -> [a] -> [a]
(?:) Nothing xs = xs
(?:) (Just x) xs = x : xs
{-# INLINE (?:) #-}

infixr ?:

authorize
  :: ByteString -- ^ Auth URL
  -> Key
  -> Maybe ByteString -- ^ Redirect URL
  -> [Scope]
  -> Maybe Nonce -- ^ An unguessable random string. It is used to protect against cross-site request forgery attacks.
  -> Response
authorize authPoint (Key cid) callbackUri ss idStr
  = responseLBS seeOther303 [(hLocation, authPoint <> renderSimpleQuery True q)] ""
  where
    addScopes = if null ss
      then id
      else (("scope", B.intercalate "," $ map scopeRep ss) :)
    nonce = (\(Nonce st) -> ("state", st)) <$> idStr
    redirect = (\r -> ("redirect_uri", r)) <$> callbackUri
    q = addScopes $ nonce ?: redirect ?: [("client_id", cid)]


authGitHub
  :: Key
  -> Maybe ByteString -- ^ Redirect URL
  -> [Scope]
  -> Maybe Nonce -- ^ An unguessable random string. It is used to protect against cross-site request forgery attacks.
  -> Response
authGitHub = authorize "https://github.com/login/oauth/authorize"


newtype Code = Code ByteString


data CallbackResponse = CallbackResponse
  { callbackResponseCode  :: Code
  , callbackResponseState :: Maybe Nonce
  }

receiveCallback :: Request -> Maybe CallbackResponse
receiveCallback r
  = CallbackResponse
      <$> (Code <$> join (lookup "code" $ queryString r))
      <*> pure (Nonce <$> join (lookup "state" $ queryString r))

acquireAccessToken
  :: Key
  -> Secret
  -> Code
  -> Maybe Text -- ^ Redirect URL
  -> Maybe Nonce
  -> IO Access
acquireAccessToken = do
  u <- parseUrl "https://github.com/login/oauth/authorize" 
  post man u
-- TODO enterprise version of OAuth

