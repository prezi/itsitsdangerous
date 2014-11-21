{-# LANGUAGE EmptyDataDecls, OverloadedStrings #-}
module ItsItsDangerous (defaultSalt, defaultSeparator, rawMessage,
                        sign, signWithSalt, signWithSeparatorAndSalt,
                        Message, Signed, Unsigned) where

import            Crypto.Hash.SHA1       (hash)
import qualified  Data.ByteString
import            Data.ByteString        (append, ByteString, spanEnd, unpack)
import qualified  Data.ByteString.Base64
import            Data.ByteString.Lazy   (toStrict)
import            Data.ByteString.Search (replace)
import            Data.Char              (ord)
import            Data.HMAC              (hmac_sha1)

type Secret    = ByteString
type Separator = ByteString
type Salt      = ByteString
type Key       = ByteString

data Signed
data Unsigned

data Message a = Message { rawMessage :: ByteString }
                 deriving (Eq, Show)

-- itsdangerous url safe version of base64 encoding/decoding
-- replace in encoded string:
--  '+' with '-'
--  '/' with '_'
-- strip '=' chars from the end of encoded string
base64encode :: ByteString -> ByteString
base64encode =
  fst . spanEnd (== (fromIntegral $ ord '='))
      . replaceStrict "/" "_" . replaceStrict "+" "-"
      . Data.ByteString.Base64.encode

replaceStrict :: ByteString -> ByteString -> ByteString -> ByteString
replaceStrict f t s = toStrict $ replace f t s

base64decode :: ByteString -> Either String ByteString
base64decode e =
  decode e
  where
  suffix = Data.ByteString.pack $ replicate (4 - (Data.ByteString.length e `mod` 4)) (fromIntegral $ ord '=')
  decode = Data.ByteString.Base64.decode . replaceStrict "/" "_"
                                         . replaceStrict "+" "-"
                                         . (`append` suffix)

derivateKey :: Salt -> Secret -> Key
derivateKey salt secret =
  hash key
  where
  key = salt `append` ("signer"::ByteString) `append` secret

defaultSalt :: ByteString
defaultSalt = "itsdangerous.Signer"

defaultSeparator :: ByteString
defaultSeparator = "."

signWithSeparatorAndSalt :: Separator -> Salt -> Secret -> ByteString -> Message Signed
signWithSeparatorAndSalt separator salt secret plain =
  Message $ plain `append` separator `append` m
  where
  c = Data.ByteString.pack $ hmac_sha1 (unpack (derivateKey salt secret)) (unpack plain)
  m = base64encode c

signWithSalt :: Salt -> Secret -> ByteString -> Message Signed
signWithSalt = signWithSeparatorAndSalt defaultSeparator

sign :: Secret -> ByteString -> Message Signed
sign = signWithSalt defaultSalt
