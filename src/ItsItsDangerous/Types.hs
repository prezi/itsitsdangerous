module ItsItsDangerous.Types where

import           Data.ByteString          (ByteString)

type Secret    = ByteString
type Separator = ByteString
type Salt      = ByteString
type Key       = ByteString
type Timestamp = Integer

type Plain     = ByteString
type Encrypted = ByteString
type Signature = ByteString

type HasExpired = Timestamp -> Bool
data UnsingError = SignatureExpired | BadSignature
                   deriving (Eq, Show)

