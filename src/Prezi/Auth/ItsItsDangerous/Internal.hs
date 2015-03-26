{-# LANGUAGE OverloadedStrings #-}
module Prezi.Auth.ItsItsDangerous.Internal where

import           Data.HMAC
import           Data.Bits              (shiftL, shiftR, (.|.))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64
import           Data.ByteString.Lazy   (toStrict)
import           Data.ByteString.Search (replace)
import qualified Data.ByteString.Unsafe as BSU
import           Data.Char              (ord)
import           Data.Int
import           Prezi.Auth.ItsItsDangerous.Types

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

rsplitE :: Separator  -- ^ String to search for.
        -> ByteString -- ^ String to search in.
        -> Either String (ByteString, ByteString) -- ^ prefix and sufix without separator
rsplitE separator string =
  case rsplit separator string of
    Nothing -> Left $ "Cannot find " ++ show separator ++ " in " ++ show string
    Just (a, b) -> Right (a, b)

-- itsdangerous url safe version of base64 encoding/decoding
-- replace in encoded string:
--  '+' with '-'
--  '/' with '_'
-- strip '=' chars from the end of encoded string
base64encode :: ByteString -> ByteString
base64encode =
    fst . BS.spanEnd (== (fromIntegral $ ord '='))
        . replaceStrict "/" "_" . replaceStrict "+" "-"
        . Data.ByteString.Base64.encode

replaceStrict :: ByteString -> ByteString -> ByteString -> ByteString
replaceStrict f t s = toStrict $ replace f t s

base64decode :: ByteString -> Either String ByteString
base64decode e =
    decode e
  where
    suffix = BS.pack $ replicate ((-BS.length e) `mod` 4) (fromIntegral $ ord '=')
    decode = Data.ByteString.Base64.decode . replaceStrict "_" "/"
                                           . replaceStrict "-" "+"
                                           . (`BS.append` suffix)

derivateKey :: KeyDerivation -> Salt -> Secret -> Key
derivateKey keyDerivation salt secret =
  case keyDerivation of
    Concat -> salt `BS.append` secret
    DjangoConcat -> salt `BS.append` ("signer" :: ByteString) `BS.append` secret
    Hmac -> BS.pack $ hmac_sha1 (BS.unpack secret) (BS.unpack salt)

bytes2int :: ByteString -> Int64
bytes2int = BS.foldl (\b a -> shiftL b 8 .|. fromIntegral a) 0

int2bytes :: Int64 -> ByteString
int2bytes =
    BS.pack . reverse . int2bytes'
  where
    int2bytes' y | y <= 0 = []
                 | otherwise = fromIntegral y : int2bytes' (shiftR y 8)

