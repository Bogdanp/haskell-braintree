{-# LANGUAGE OverloadedStrings #-}

module Web.Braintree.Types
  ( FromXML(..)
  , ClientToken(..)
  ) where

import Control.Monad.Trans.Resource (MonadThrow)
import Data.ByteString (ByteString)
import Data.Conduit (Consumer)
import Data.Text.Encoding (encodeUtf8)
import Data.XML.Types (Event)
import Text.XML.Stream.Parse (content, force, tagNoAttr)

class FromXML a where
  parseXML
    :: MonadThrow m
    => Consumer Event m a

newtype Value = Value
  { getValue :: ByteString
  } deriving (Eq, Ord, Read, Show)

instance FromXML Value where
  parseXML =
    force "expected 'value'" $
    tagNoAttr "value" (Value . encodeUtf8 <$> content)

-- | A token for the Javascript client.
newtype ClientToken = ClientToken
  { getClientToken :: ByteString
  } deriving (Eq, Ord, Read, Show)

instance FromXML ClientToken where
  parseXML =
    force "expected 'client-token'" $
    tagNoAttr "client-token" (ClientToken . getValue <$> parseXML)
