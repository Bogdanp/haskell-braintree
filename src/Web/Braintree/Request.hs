{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Braintree.Request
  ( Method(..)
  , BraintreeParameter(..)
  , BraintreeRequest(..)
  , BraintreeReturn
  , toXML
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as C

-- | HTTP Method
--
-- These are the only methods supported by the API.
data Method
  = DELETE
  | GET
  | POST
  | PUT
  deriving (Eq, Ord, Read, Show)

-- | Braintree Request Parameters
data BraintreeParameter
  = BTDict [(ByteString, BraintreeParameter)]
  | BTList [BraintreeParameter]
  | BTBool Bool
  | BTInt Int
  | BTStr ByteString

escapeXML :: ByteString -> ByteString
escapeXML = C.concatMap escape
  where
    escape '&' = "&amp;"
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '"' = "&quot;"
    escape '\'' = "&apos;"
    escape c = C.singleton c

-- | Convert a BraintreeParameter to an XML bytestring.
toXML :: BraintreeParameter -> ByteString
toXML (BTBool True) = "true"
toXML (BTBool False) = "false"
toXML (BTInt n) = pack . show $ n
toXML (BTStr s) = escapeXML s
toXML (BTList xs) = foldl (<>) "" $ map item xs
  where
    item x = "<item>" <> toXML x <> "</item>"
toXML (BTDict xs) = foldl (<>) "" $ map kv xs
  where
    kv (k, v@(BTBool _)) = tag k "boolean" $ toXML v
    kv (k, v@(BTInt _)) = tag k "integer" $ toXML v
    kv (k, v@(BTStr _)) = tag' k $ toXML v
    kv (k, v@(BTList _)) = tag k "array" $ toXML v
    kv (k, v@(BTDict _)) = tag' k $ toXML v
    tag k t v = "<" <> k <> " type=\"" <> t <> "\">" <> v <> "</" <> k <> ">"
    tag' k v = "<" <> k <> ">" <> v <> "</" <> k <> ">"

-- | Braintree Request
data BraintreeRequest a = BraintreeRequest
  { method :: Method -- ^ the API request method (one of DELETE, GET, POST or PUT)
  , endpoint :: ByteString -- ^ the API endpoint to request
  , parameters :: BraintreeParameter -- ^ the request parameters
  }

-- | Return type of a Braintree Request
type family BraintreeReturn a :: *
