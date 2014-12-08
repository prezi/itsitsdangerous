{-# LANGUAGE OverloadedStrings #-}
module ItsItsDangerous
       (defaultSalt, defaultSeparator, sign, signWithSalt,
        signWithSeparatorAndSalt, signWithTimestampAndSeparator,
        signWithTimestamp, unsignTimestampped,
        unsignTimestamppedWithSeparatorAndSalt)
       where

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.HMAC                (hmac_sha1)
import           ItsItsDangerous.Internal (attachTimestamp, base64encode,
                                           derivateKey,
                                           extractMessageAndTimestamp, rsplit)
import           ItsItsDangerous.Types    (Encrypted, HasExpired, Plain, Salt,
                                           Secret, Separator, Signature,
                                           Timestamp, UnsingError (..))

defaultSalt :: ByteString
defaultSalt = "itsdangerous.Signer"

defaultSeparator :: ByteString
defaultSeparator = "."

signature :: Salt -> Secret -> Plain -> Signature
signature salt secret plain =
    base64encode . BS.pack $ hmac_sha1 (BS.unpack (derivateKey salt secret)) (BS.unpack plain)

signWithSeparatorAndSalt :: Separator -> Salt -> Secret -> Plain -> Encrypted
signWithSeparatorAndSalt separator salt secret plain =
    plain `BS.append` separator `BS.append` signature salt secret plain

signWithSalt :: Salt -> Secret -> ByteString -> Encrypted
signWithSalt = signWithSeparatorAndSalt defaultSeparator

sign :: Secret -> ByteString -> Encrypted
sign = signWithSalt defaultSalt

signWithTimestampAndSeparator :: Timestamp
                              -> Separator
                              -> Secret
                              -> Plain
                              -> Encrypted
signWithTimestampAndSeparator timestamp separator secret plain =
    let plain' =
            attachTimestamp timestamp separator plain
    in signWithSeparatorAndSalt separator defaultSalt secret plain'

signWithTimestamp :: Timestamp -> Secret -> Plain -> Encrypted
signWithTimestamp timestamp = signWithTimestampAndSeparator timestamp defaultSeparator

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


unsignTimestamppedWithSeparatorAndSalt :: HasExpired
                                       -> Separator
                                       -> Salt
                                       -> Secret
                                       -> ByteString
                                       -> Either UnsingError Plain
unsignTimestamppedWithSeparatorAndSalt hasExpired separator salt secret encrypted = do
    plain <- unsign separator salt secret encrypted
    let m = extractMessageAndTimestamp separator $ plain
    maybe
        (Left BadSignature)
        (\(p, t) ->
              if hasExpired t p
                  then Left SignatureExpired
                  else return p)
        m

unsignTimestampped :: HasExpired
                   -> Secret
                   -> Encrypted
                   -> Either UnsingError Plain
unsignTimestampped hasExpired =
    unsignTimestamppedWithSeparatorAndSalt
        hasExpired
        defaultSeparator
        defaultSalt
