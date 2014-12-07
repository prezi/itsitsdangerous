{-# LANGUAGE CPP, EmptyDataDecls, OverloadedStrings #-}
module ItsItsDangerous
       (attachTimestamp, defaultSalt, defaultSeparator, sign, signWithSalt,
        signWithSeparatorAndSalt, signWithTimestampAndSeparator,
        extractMessageAndTimestamp, Separator, Secret, base64encode,
        base64decode, int2bytes, base64encode, unsignTimestampped, signWithTimestamp)
       where

import           Control.Applicative      ((<$>))
import           Control.Monad            (mfilter)
import           Crypto.Hash.SHA1         (hash)
import           Data.Bits                (shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString          (ByteString, append, breakSubstring,
                                           pack, spanEnd, unpack)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Unsafe   as BSU
import           Data.ByteString.Lazy     (toStrict)
import           Data.ByteString.Search   (replace)
import           Data.ByteString.UTF8     (fromString, toString)
import           Data.Char                (isDigit, ord)
import           Data.HMAC                (hmac_sha1)
import           Data.Maybe               (maybe)
import           Text.Read                (readMaybe)

type Secret    = ByteString
type Separator = ByteString
type Salt      = ByteString
type Key       = ByteString

type Plain     = ByteString
type Encrypted = ByteString

-- data Message = Message
--     { plain :: Plain
--     , encrypted :: Encrypted
--     }
-- 
-- data MessageWithTimestamp = {message :: Message
--                             ,timestamp :: Timestamp
--                             }

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

signature :: Salt -> Secret -> ByteString -> ByteString
signature salt secret plain =
    base64encode . BS.pack $ hmac_sha1 (unpack (derivateKey salt secret)) (unpack plain)

signWithSeparatorAndSalt :: Separator -> Salt -> Secret -> ByteString -> Encrypted
signWithSeparatorAndSalt separator salt secret plain =
    plain `append` separator `append` signature salt secret plain

signWithSalt :: Salt -> Secret -> ByteString -> Encrypted
signWithSalt = signWithSeparatorAndSalt defaultSeparator

sign :: Secret -> ByteString -> Encrypted
sign = signWithSalt defaultSalt

type Timestamp = Integer

epoch :: Timestamp
epoch = 1293840000

bytes2int :: ByteString -> Integer
bytes2int = BS.foldl (\b a -> shiftL b 8 .|. (toInteger a)) 0

int2bytes :: Integer -> ByteString
int2bytes = pack . reverse . int2bytes'
  where
  int2bytes' y | y <= 0 = []
               | otherwise = (fromInteger $ y .&. 0xff) : int2bytes' (shiftR y 8)

attachTimestamp :: Timestamp -> Separator -> ByteString -> ByteString
attachTimestamp timestamp separator plain =
    plain `append` separator `append`
    (base64encode .  int2bytes .  (\x -> x - epoch) $ timestamp)

rsplit :: Separator  -- ^ String to search for.
       -> ByteString -- ^ String to search in.
       -> Maybe (ByteString, ByteString) -- ^ prefix and sufix without separator
rsplit separator buffer | separator `seq` buffer `seq` False = undefined
rsplit separator buffer = do
    i <- rsplit' buffer 0 Nothing
    return (BS.take i buffer, BS.drop (i + BS.length separator) buffer)
  where
    rsplit' :: ByteString -> Int -> Maybe Int -> Maybe Int
    rsplit' b c i
        | BS.null b                   = i
        | separator `BS.isPrefixOf` b = rsplit' (BSU.unsafeTail b) (c+1) (Just c)
        | otherwise                   = rsplit' (BSU.unsafeTail b) (c+1) i


extractMessageAndTimestamp :: Separator -> ByteString -> Maybe (Plain, Timestamp)
extractMessageAndTimestamp separator plain = do
    (p, t) <- rsplit separator plain
    t' <- either (const Nothing) Just .  base64decode $ t
    return . ((,) p) . (epoch +) . bytes2int $ t'

signWithTimestampAndSeparator :: Timestamp -> Separator -> Secret -> Plain -> Encrypted
signWithTimestampAndSeparator timestamp separator secret plain =
    let plain' =
            attachTimestamp timestamp separator plain
    in signWithSeparatorAndSalt separator defaultSalt secret plain'

signWithTimestamp :: Timestamp -> Secret -> Plain -> Encrypted
signWithTimestamp timestamp = signWithTimestampAndSeparator timestamp defaultSeparator

type HasExpired = Timestamp -> Bool
data UnsingError = SignatureExpired | BadSignature
                   deriving (Eq, Show)

unsign :: Separator -> Salt -> Secret -> Encrypted -> Either UnsingError Plain
unsign separator salt secret encrypted =
    maybe (Left BadSignature)
          (\(v, s) -> if validate v s
                          then Right v
                          else Left BadSignature)
          (rsplit separator encrypted)
  where
    validate :: Plain -> Encrypted -> Bool
    validate plain = (signature salt secret plain ==)


unsignTimestamppedWithSeparatorAndSalt :: HasExpired -> Separator -> Salt -> Secret -> ByteString -> Either UnsingError Plain
unsignTimestamppedWithSeparatorAndSalt hasExpired separator salt secret encrypted = do -- unsign separator salt secret encrypted
    plain <- unsign separator salt secret encrypted
    let m = extractMessageAndTimestamp separator $ plain
    maybe
        (Left BadSignature)
        (\(p, t) ->
              if hasExpired t
                  then Left SignatureExpired
                  else return p)
        m

unsignTimestampped :: HasExpired -> Secret -> Encrypted -> Either UnsingError Plain
unsignTimestampped hasExpired = unsignTimestamppedWithSeparatorAndSalt hasExpired defaultSeparator defaultSalt
