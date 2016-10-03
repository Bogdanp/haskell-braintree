{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Braintree.Client.Conduit
  ( braintree
  ) where

import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Data.ByteString.Char8 (pack)
import qualified Data.CaseInsensitive as CI
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Monoid ((<>))
import Network.HTTP.Client hiding (setRequestIgnoreStatus)
import Network.HTTP.Simple
import qualified Network.HTTP.Types as NHT
import Text.XML (sinkDoc)
import Text.XML.Stream.Parse
import Web.Braintree.Client
import qualified Web.Braintree.Client as C
import Web.Braintree.Types

debug :: Bool
debug = False

braintree
  :: (FromXML (BraintreeReturn a))
  => Manager
  -> BraintreeConfig
  -> BraintreeRequest a
  -> IO (Either BraintreeError (BraintreeReturn a))
braintree manager conf@BraintreeConfig {..} req@BraintreeRequest {..} = do
  let request =
        addRequestBody $
        addRequestHeaders $
        setRequestManager manager $
        setRequestIgnoreStatus $
        setRequestBasicAuth
          (getClientId clientId)
          (getClientSecret clientSecret) $
        setRequestMethod (convertMethod method) $
        setRequestHost (requestHost conf) $
        setRequestPath (requestPath conf req) $
        setRequestSecure True $ setRequestPort 443 $ defaultRequest
  response <- perform request
  case response of
    Left XmlException {..} ->
      return . Left . BraintreeError ParseError $ pack xmlErrorMessage
    Right x -> return x
  where
    perform request =
      try $
      runResourceT $
      httpSink request $ \response -> do
        when debug $ liftIO $ print response
        let status = NHT.statusCode $ responseStatus response
        if not $ isErrorCode status
          then Right <$> (parseBytes def $= parseXML)
          else (Left . BraintreeError (APIError . errorCodeFromStatus $ status)) <$>
               (CL.fold (<>) "")
    addRequestBody =
      if method == GET || method == DELETE
        then id
        else setRequestBody $ RequestBodyBS $ toXML parameters
    addRequestHeader' req (h, v) = addRequestHeader (CI.mk h) v req
    addRequestHeaders req = foldl addRequestHeader' req (C.requestHeaders conf)

convertMethod :: Method -> NHT.Method
convertMethod DELETE = "DELETE"
convertMethod GET = "GET"
convertMethod POST = "POST"
convertMethod PUT = "PUT"
