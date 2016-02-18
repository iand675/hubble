{-# LANGUAGE OverloadedStrings #-}
module GitHub.Miscellaneous.RateLimit where
import Control.Monad.Trans
import GitHub.Internal
import GitHub.Types

getRateLimit :: (GitHubM s m) => m (GitHubResponse RateLimitInfo)
getRateLimit = get (v3 json) ["rate_limit"] []

