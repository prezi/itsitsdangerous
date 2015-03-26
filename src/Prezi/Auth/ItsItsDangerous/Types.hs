module Prezi.Auth.ItsItsDangerous.Types where

import Data.ByteString          (ByteString)
import System.Posix.Types

type Secret    = ByteString
type Separator = ByteString
type Salt      = ByteString
type Key       = ByteString
type Timestamp = EpochTime

type Plain     = ByteString
type Encrypted = ByteString
type Signature = ByteString

type HasExpired = Timestamp -> Plain -> Bool
data UnsingError = SignatureExpired | BadSignature
                   deriving (Eq, Show)

data SignedMessage
  = SignedMessage
    { plainMessage :: ByteString
    , timestampedMessageBase64 :: ByteString -- and potentially zlibbed
    , timestamp :: EpochTime                 -- from unix epoch
    , signatureBase64 :: ByteString
    }
  deriving (Eq, Show)

data KeyDerivation
  = Concat
  | DjangoConcat
  | Hmac
  deriving (Eq, Show)
