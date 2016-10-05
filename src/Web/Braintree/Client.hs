{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Braintree.Client
  ( module Web.Braintree.Error
  , module Web.Braintree.Request
  , Environment(..)
  , MerchantId(..)
  , ClientId(..)
  , ClientSecret(..)
  , BraintreeConfig(..)
  , APIVersion(..)
  , requestHeaders
  , requestHost
  , requestPath
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Monoid ((<>))
import Web.Braintree.Error
import Web.Braintree.Request

-- | Braintree Environment
data Environment
  = Sandbox
  | Production
  deriving (Eq, Ord, Read, Show)

-- | A Braintree merchant id.
newtype MerchantId = MerchantId
  { getMerchantId :: ByteString
  } deriving (Eq, Ord, Read, Show)

-- | A Braintree client id.
newtype ClientId = ClientId
  { getClientId :: ByteString
  } deriving (Eq, Ord, Read, Show)

-- | A Braintree client secret.
newtype ClientSecret = ClientSecret
  { getClientSecret :: ByteString
  } deriving (Eq, Ord, Read, Show)

-- | Braintree config
data BraintreeConfig = BraintreeConfig
  { environment :: Environment
  , merchantId :: MerchantId
  , clientId :: ClientId
  , clientSecret :: ClientSecret
  } deriving (Eq, Ord, Read, Show)

-- | API version
data APIVersion =
  V4 -- ^ The Braintree API version used by this package.
  deriving (Eq, Ord)

instance Show APIVersion where
  show V4 = "4"

-- | Generate the basic set of request headers for a BraintreeConfig.
requestHeaders :: BraintreeConfig -> [(ByteString, ByteString)]
requestHeaders conf@BraintreeConfig {..} =
  [ ("accept", "application/xml")
  , ("content-type", "application/xml")
  , ("user-agent", "Braintree Haskell")
  , ("x-apiversion", pack . show $ V4)
  ]

-- | Retrieve the API host for a BraintreeConfig.
requestHost :: BraintreeConfig -> ByteString
requestHost BraintreeConfig {..} = environmentRoot environment

environmentRoot :: Environment -> ByteString
environmentRoot Sandbox = "api.sandbox.braintreegateway.com"
environmentRoot Production = "api.braintreegateway.com"

-- | Retrieve the API request path for a config and request.
requestPath :: BraintreeConfig -> BraintreeRequest a -> ByteString
requestPath BraintreeConfig {..} BraintreeRequest {..} =
  "/merchants/" <> getMerchantId merchantId <> "/" <> endpoint
