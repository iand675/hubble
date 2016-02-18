{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
module GitHub.Internal where
import Blaze.ByteString.Builder (toByteString)
import Control.Exception
import Control.Lens
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Aeson (ToJSON, FromJSON, eitherDecode')
import Data.Aeson.TH
import Data.Char (isUpper, toLower)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.CaseInsensitive
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Proxy
import Data.String
import Data.Word
import Language.Haskell.TH (Q, Dec, Name, nameBase)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.HTTP.Types.Header

import GitHub.Types

newtype MissingHeader = MissingHeader (CI ByteString)
  deriving (Show)

instance Exception MissingHeader

newtype InvalidPositiveNumber = InvalidPositiveNumber ByteString
  deriving (Show)

instance Exception InvalidPositiveNumber

newtype ETag = ETag ByteString
  deriving (Show)

data ClientSettings = ClientSettings
  { clientSettingsUserAgent :: ByteString }

github :: ClientSettings -> IO GitHubManager
github s = do
  r <- parseUrl "https://api.github.com"
  m <- newManager tlsManagerSettings
  let r' = r { requestHeaders = (hUserAgent, clientSettingsUserAgent s) : requestHeaders r }
  return $ GitHubManager r' m []

testGitHub :: ReaderT GitHubManager IO a -> IO a
testGitHub m = do
  g <- github (ClientSettings "iand675")
  runReaderT m g

{-
githubEnterprise :: Text -> Either ParseError URI
githubEnterprise = undefined
-}

data GitHubManager = GitHubManager
  { gitHubManagerBaseReq    :: Request
  , gitHubManagerManager    :: Manager
  , gitHubManagerBaseParams :: [(ByteString, Maybe ByteString)]
  }

makeClassy ''GitHubManager
makeFields ''GitHubManager

data GitHubResponse a = GitHubResponse
  { gitHubResponseRateLimit           :: Word
  , gitHubResponseRateLimitRemaining  :: Word
  , gitHubResponseRateLimitReset      :: Word64
  , gitHubResponseETag                :: Maybe ETag
  , gitHubResponseOAuthScopes         :: [T.Text]
  , gitHubResponseAcceptedOAuthScopes :: [T.Text]
  -- , gitHubResponseHeaders            :: ResponseHeaders
  , gitHubResponseValue              :: a
  } deriving (Show)

makeFields ''GitHubResponse

readPositive :: Num a => ByteString -> a
readPositive as
  | B.null as = throw $ InvalidPositiveNumber as
  | otherwise = loop 0 0 as
  where
    loop !i !n !ps
      | B.null ps = end i n ps
      | otherwise =
          case B.unsafeHead ps of
            w | w >= 0x30
             && w <= 0x39 -> loop (i + 1)
                                  (n * 10 + (fromIntegral w - 0x30))
                                  (B.unsafeTail ps)
              | otherwise -> end i n ps

    end 0 _ _  = throw $ InvalidPositiveNumber as
    end _ n ps = if B.null ps
      then n
      else throw $ InvalidPositiveNumber ps

mandatoryHeader :: CI ByteString -> ResponseHeaders -> ByteString
mandatoryHeader k hs = case lookup k hs of
  Nothing -> throw $ MissingHeader k
  Just v -> v

ghResponse :: Response a -> GitHubResponse a
ghResponse r = GitHubResponse rl rlRemaining rlReset etag scopes acceptedScopes val
  where
    hs = responseHeaders r
    rl = readPositive $ mandatoryHeader "X-RateLimit-Limit" hs
    rlRemaining = readPositive $ mandatoryHeader "X-RateLimit-Remaining" hs
    rlReset = readPositive $ mandatoryHeader "X-RateLimit-Reset" hs
    etag = ETag <$> lookup "ETag" hs
    scopes = T.splitOn ", " $ decodeUtf8 $ mandatoryHeader "X-OAuth-Scopes" hs
    acceptedScopes = T.splitOn ", " $ decodeUtf8 $ mandatoryHeader "X-Accepted-OAuth-Scopes" hs
    val = responseBody r

class Version a where
  version :: Proxy a -> ByteString

data V3

instance Version V3 where
  version = const "v3"

class MediaType a where
  mediaType :: f a -> ByteString

data Raw

instance MediaType Raw where
  mediaType = const "raw"

data Text

instance MediaType Text where
  mediaType = const "text"

data Html

instance MediaType Html where
  mediaType = const "html"

data Full

instance MediaType Full where
  mediaType = const "full"

data Json

instance MediaType Json where
  mediaType = const "json"

data Diff

instance MediaType Diff where
  mediaType = const "diff"

data Patch

instance MediaType Patch where
  mediaType = const "patch"

data Base64

instance MediaType Base64 where
  mediaType = const "base64"

class MediaType a => CommentBodyContents a
instance CommentBodyContents Raw
instance CommentBodyContents Text
instance CommentBodyContents Html
instance CommentBodyContents Full

class MediaType a => GitBlobContents a
instance GitBlobContents Json
instance GitBlobContents Raw

class MediaType a => ComparisonContents a
instance ComparisonContents Diff
instance ComparisonContents Patch

class MediaType a => RepositoryContents a
instance RepositoryContents Raw
instance RepositoryContents Html

class MediaType a => GistsContents a
instance GistsContents Raw
instance GistsContents Base64

v3 :: Proxy t -> Proxy (V3, t)
v3 = const Proxy

json :: Proxy Json
json = Proxy

type GitHubM s m = (MonadIO m, MonadReader s m, HasGitHubManager s)

get :: (MonadIO m, MonadReader s m, HasGitHubManager s, Version v, MediaType t, FromJSON a, Show a)
    => Proxy (v, t)
    -> [T.Text]
    -> [(ByteString, Maybe ByteString)]
    -> m (GitHubResponse a)
get proxy p q = do
  m <- view gitHubManager
  let man = gitHubManagerManager m
  liftIO $ do
    let req' = setQueryString (q ++ gitHubManagerBaseParams m) $ gitHubManagerBaseReq m
    respLbs <- httpLbs (req' { path = toByteString $ encodePathSegments p }) man
    case eitherDecode' (responseBody respLbs) of
      -- TODO handle better than fail
      Left err -> print respLbs >> fail err
      Right x -> do
        let r' = respLbs { responseBody = x }
        print r'
        return $ ghResponse r'

{-
put :: (GitHubMonad m, Version v, MediaType t) => Proxy (v, t) -> Path -> a -> m
post :: (GitHubMonad m, Version v, MediaType t) => Proxy (v, t) -> Path -> a -> m
patch :: (GitHubMonad m, Version v, MediaType t) => Proxy (v, t) -> Path -> a -> m
delete :: (GitHubMonad m, Version v, MediaType t) => Proxy (v, t) -> Path -> m
-}

auth :: GitHubM s m => Auth -> m a -> m a
auth a = local (over gitHubManager (addAuth a))

headers :: Lens' Request RequestHeaders
headers = lens requestHeaders (\r x -> r { requestHeaders = x })
{-# INLINE headers #-}

addAuth :: Auth -> GitHubManager -> GitHubManager
addAuth a man = case a of
  AuthToken (Token t) ->
    man & baseReq . headers %~ ((hAuthorization, "token " <> t) :)

  AuthKeyAndSecret (Key k) (Secret s) ->
    man & baseParams %~ ([("client_id", Just k), ("client_secret", Just s)] ++)

  BasicAuth (Username u) (Password p) ->
    man & baseReq %~ applyBasicAuth (encodeUtf8 u) (encodeUtf8 p)

jsonize :: Name -> Q [Dec]
jsonize n = deriveJSON (defaultOptions
  { fieldLabelModifier = \str -> case drop (length $ nameBase n) str of
      [] -> []
      (c:cs) -> let n = foldr (\c cs -> if isUpper c then '_' : toLower c : cs else c : cs) "" (toLower c : cs) in if last n == '_'
        then Prelude.init n
        else n
  }) n

opt :: (a -> b) -> (k, Maybe a) -> [(k, b)]
opt _ (_, Nothing) ys = ys
opt f (k, Just x)  ys = (k, f x) : ys
{-# INLINE opt #-}
