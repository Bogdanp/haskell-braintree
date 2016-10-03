{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Braintree.ClientToken
  ( GenerateClientToken
  , generateClientToken
  ) where

import Data.ByteString (ByteString)
import Web.Braintree.Request
import Web.Braintree.Types

data GenerateClientToken

type instance BraintreeReturn GenerateClientToken = ClientToken

-- | Generate a Braintree Client Token.
generateClientToken :: BraintreeRequest GenerateClientToken
generateClientToken =
  BraintreeRequest POST "client_token" (BTDict [("client_token", BTDict [])])
