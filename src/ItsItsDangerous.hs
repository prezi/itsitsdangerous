{-# LANGUAGE EmptyDataDecls, OverloadedStrings #-}
module ItsItsDangerous
       (attachTimestamp, defaultSalt, defaultSeparator, rawMessage, sign, signWithSalt,
        signWithSeparatorAndSalt, signWithTimestampAndSeparator, Message,
        extractTimestamp, Signed, Unsigned, Separator, Secret, base64encode,
        base64decode, int2bytes, base64encode)
       where

import            Crypto.Hash.SHA1       (hash)
import            Control.Applicative    ((<$>))
import            Control.Monad          (mfilter)
import qualified  Data.ByteString        as BS
import            Data.ByteString        (append, ByteString, pack, spanEnd, breakSubstring, unpack)
import qualified  Data.ByteString.Base64
import            Data.ByteString.Lazy   (toStrict)
import            Data.ByteString.Search (replace)
import            Data.ByteString.UTF8   (fromString, toString)
import            Data.Bits              (shiftL, shiftR, (.|.), (.&.))
import            Data.Char              (ord, isDigit)
import            Data.HMAC              (hmac_sha1)
import            Text.Read              (readMaybe)

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
  suffix = BS.pack $ replicate (4 - (BS.length e `mod` 4)) (fromIntegral $ ord '=')
  decode = Data.ByteString.Base64.decode . replaceStrict "_" "/"
                                         . replaceStrict "-" "+"
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
  c = BS.pack $ hmac_sha1 (unpack (derivateKey salt secret)) (unpack plain)
  m = base64encode c

signWithSalt :: Salt -> Secret -> ByteString -> Message Signed
signWithSalt = signWithSeparatorAndSalt defaultSeparator

sign :: Secret -> ByteString -> Message Signed
sign = signWithSalt defaultSalt

epoch :: Integer
epoch = 1293840000

attachTimestamp :: Integer -> Separator -> ByteString -> ByteString
attachTimestamp timestamp separator plain =
  plain `append` separator `append` (base64encode . int2bytes . (\x -> x - epoch) $ timestamp)

bytes2int :: ByteString -> Integer
bytes2int = BS.foldl (\b a -> shiftL b 8 .|. (toInteger a)) 0

int2bytes :: Integer -> ByteString
int2bytes = pack . reverse . int2bytes'
  where
  int2bytes' y | y <= 0 = []
               | otherwise = (fromInteger $ y .&. 0xff) : int2bytes' (shiftR y 8)

extractTimestamp :: Separator -> ByteString -> ByteString -> Maybe Integer
extractTimestamp separator plain plainWithSeparator = do
  let t = BS.drop (BS.length separator + BS.length plain) $ plainWithSeparator
  t' <- either (const Nothing) Just . base64decode $ t
  return . (epoch +) . bytes2int $ t'

signWithTimestampAndSeparator :: Integer -> Separator -> Secret -> ByteString -> Message Signed
signWithTimestampAndSeparator timestamp separator secret plain =
    let plain' =
            attachTimestamp timestamp separator plain
    in signWithSeparatorAndSalt separator defaultSalt secret plain'
