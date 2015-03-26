{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Prezi.Auth.ItsItsDangerous
  ( module Prezi.Auth.ItsItsDangerous.Types
  , sign, unsign
  )
  where

import qualified Codec.Compression.Zlib as Zlib
import           Control.Applicative
import           Control.Exception
import           Control.Monad.Except
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import           Data.HMAC
import           Data.Int
import           Data.SecureMem
import           Foreign.C.Types
import           Prezi.Auth.ItsItsDangerous.Internal
import           Prezi.Auth.ItsItsDangerous.Types

_ITS_DANGEROUS_EPOCH :: Int64
_ITS_DANGEROUS_EPOCH = 1293840000

createSignature :: KeyDerivation -> Salt -> Secret -> Plain -> Signature
createSignature keyDerivation salt secret plain =
    base64encode . BS.pack $ hmac_sha1 (BS.unpack (derivateKey keyDerivation salt secret)) (BS.unpack plain)

dump :: Separator -> SignedMessage -> Encrypted
dump separator SignedMessage{..} = BS.intercalate separator [timestampedMessageBase64, signatureBase64]

sign :: Separator -> KeyDerivation -> Salt -> Secret -> Timestamp -> Plain -> Encrypted
sign separator keyDerivation salt secret (CTime timestamp) plain =
  let
    timestampByteString = base64encode . int2bytes $ timestamp - _ITS_DANGEROUS_EPOCH
    timestampedMessage = BS.intercalate separator [base64encode plain, timestampByteString]
    signedMessage =
      SignedMessage
        { plainMessage = plain
        , timestampedMessageBase64 = timestampedMessage
        , timestamp = CTime timestamp
        , signatureBase64 = createSignature keyDerivation salt secret timestampedMessage
        }
  in dump separator signedMessage

load :: Separator -> Encrypted -> ExceptT String IO SignedMessage
load separator encrypted = do
  (timestampedMessage, signature) <- liftE $ rsplitE separator encrypted
  (deflatedBase64, timestampBase64) <- liftE $ rsplitE separator timestampedMessage
  plain <- base64decodeAndDecompressIfNecessary deflatedBase64
  timestampEpoch <- liftE $ CTime . (_ITS_DANGEROUS_EPOCH +) . bytes2int <$> base64decode timestampBase64
  return
    SignedMessage
      { plainMessage = plain
      , timestampedMessageBase64 = timestampedMessage
      , timestamp = timestampEpoch
      , signatureBase64 = signature
      }

liftE :: Monad m => Either e a -> ExceptT e m a
liftE e = ExceptT (return e)

unsign :: Separator -> KeyDerivation -> Salt -> Secret -> Encrypted -> IO (Either String (Plain, Timestamp))
unsign separator keyDerivation salt secret encrypted = runExceptT $ do
  SignedMessage{..} <- load separator encrypted
  if validate timestampedMessageBase64 signatureBase64
    then do
      return (plainMessage, timestamp)
    else throwError $ show keyDerivation ++ " validation failed"
  where
    validate :: Plain -> Encrypted -> Bool
    validate plain enc =
        (toSecureMem . createSignature keyDerivation salt secret $ plain) == toSecureMem enc

base64decodeAndDecompressIfNecessary :: BS.ByteString -> ExceptT String IO BS.ByteString
base64decodeAndDecompressIfNecessary string
  | Just (46, inflatedBase64) <- BS.uncons string = do
    inflated <- liftE $ base64decode inflatedBase64
    eDecompressed <- lift . try . evaluate . BSL.toStrict . Zlib.decompress $ BSL.fromStrict inflated
    case eDecompressed :: Either SomeException BS.ByteString of
      Left exception -> throwError (show exception)
      Right bs -> return bs
  | otherwise = liftE $ base64decode string
