{-# LANGUAGE OverloadedStrings #-}
module ItsItsDangerous.Internal where

import           Crypto.Hash.SHA1       (hash)
import           Data.Bits              (shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64
import           Data.ByteString.Lazy   (toStrict)
import           Data.ByteString.Search (replace)
import qualified Data.ByteString.Unsafe as BSU
import           Data.Char              (ord)
import           ItsItsDangerous.Types  (Key, Plain, Salt, Secret, Separator,
                                         Timestamp)

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
    suffix = BS.pack $ replicate (4 - (BS.length e `mod` 4)) (fromIntegral $ ord '=')
    decode = Data.ByteString.Base64.decode . replaceStrict "_" "/"
                                           . replaceStrict "-" "+"
                                           . (`BS.append` suffix)

derivateKey :: Salt -> Secret -> Key
derivateKey salt secret =
    hash key
  where
    key = salt `BS.append` ("signer"::ByteString) `BS.append` secret

bytes2int :: ByteString -> Integer
bytes2int = BS.foldl (\b a -> shiftL b 8 .|. toInteger a) 0

int2bytes :: Integer -> ByteString
int2bytes =
    BS.pack . reverse . int2bytes'
  where
    int2bytes' y | y <= 0 = []
                 | otherwise = fromInteger (y .&. 0xff) : int2bytes' (shiftR y 8)

epoch :: Timestamp
epoch = 1293840000

attachTimestamp :: Timestamp -> Separator -> ByteString -> ByteString
attachTimestamp timestamp separator plain =
    plain `BS.append` separator `BS.append`
    (base64encode .  int2bytes .  (\x -> x - epoch) $ timestamp)

extractMessageAndTimestamp :: Separator -> ByteString -> Maybe (Plain, Timestamp)
extractMessageAndTimestamp separator plain = do
    (p, t) <- rsplit separator plain
    t' <- either (const Nothing) Just .  base64decode $ t
    return . (,) p . (epoch +) . bytes2int $ t'

