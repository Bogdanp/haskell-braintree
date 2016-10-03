module Web.Braintree.Error
  ( BraintreeErrorCode(..)
  , BraintreeErrorKind(..)
  , BraintreeError(..)
  , errorCodeFromStatus
  , isErrorCode
  ) where

import Data.ByteString (ByteString)

-- | Braintree error codes
data BraintreeErrorCode
  = BadRequest -- ^ 400
  | Unauthenticated -- ^ 401
  | Unauthorized -- ^ 403
  | NotFound -- ^ 404
  | Unprocessable -- ^ 422
  | UpgradeRequired -- ^ 426
  | TooManyRequests -- ^ 429
  | Internal -- ^ >= 500
  | Unknown
  deriving (Eq, Ord, Read, Show)

-- | Map HTTP error codes to BraintreeErrorCode values.
errorCodeFromStatus :: Int -> BraintreeErrorCode
errorCodeFromStatus 400 = BadRequest
errorCodeFromStatus 401 = Unauthenticated
errorCodeFromStatus 403 = Unauthorized
errorCodeFromStatus 404 = NotFound
errorCodeFromStatus 422 = Unprocessable
errorCodeFromStatus 426 = UpgradeRequired
errorCodeFromStatus 429 = TooManyRequests
errorCodeFromStatus 500 = Internal
errorCodeFromStatus _ = Unknown

-- | Returns True if the given status code represents an erroneous
-- HTTP response.
isErrorCode :: Int -> Bool
isErrorCode = not . flip elem [200, 201]

-- | Braintree Error Kind
data BraintreeErrorKind
  = APIError BraintreeErrorCode
  | ParseError
  deriving (Eq, Ord, Read, Show)

-- | Braintree Error
data BraintreeError = BraintreeError
  { errorKind :: BraintreeErrorKind
  , errorMessage :: ByteString
  } deriving (Eq, Ord, Read, Show)
