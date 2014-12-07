{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString          (ByteString)
import           Data.ByteString.UTF8     (fromString)
import           Foreign.Python           (PyObject, callObject, fromPy,
                                           getAttr, importModule, initialize,
                                           toPy)
import           ItsItsDangerous          (defaultSalt, defaultSeparator, sign,
                                           signWithSeparatorAndSalt,
                                           signWithTimestamp,
                                           signWithTimestampAndSeparator,
                                           unsignTimestampped)
import           ItsItsDangerous.Internal (extractMessageAndTimestamp)
import           ItsItsDangerous.Types    (Secret, Separator)
import           System.IO.Unsafe         (unsafePerformIO)
import           Test.QuickCheck          (Arbitrary, arbitrary)
import           Test.Tasty               (TestTree, defaultMain, testGroup)
import           Test.Tasty.QuickCheck    (testProperty)

instance Arbitrary ByteString where
  arbitrary   = fmap fromString arbitrary

data OrigSigner = TimestampSigner | Signer
                  deriving (Show)

constructSignerInstance :: OrigSigner -> Separator -> Secret -> IO PyObject
constructSignerInstance signer separator secret = do
    let signer' = show signer
    initialize False
    itsdangerous <- importModule "itsdangerous"
    signerClass <- getAttr itsdangerous signer'
    secret' <- toPy secret
    salt' <- toPy defaultSalt
    separator' <- toPy separator
    callObject signerClass [secret', salt', separator'] []

signOrigWithSigner :: OrigSigner
                   -> Separator
                   -> Secret
                   -> ByteString
                   -> ByteString
signOrigWithSigner signer separator secret plain = unsafePerformIO $ do
    signerInstance <- constructSignerInstance signer separator secret
    plain' <- toPy plain
    signMethod <- getAttr signerInstance "sign"
    callObject signMethod [plain'] [] >>= fromPy

unsingOrigWithSigner :: OrigSigner
                     -> Separator
                     -> Secret
                     -> ByteString
                     -> ByteString
unsingOrigWithSigner signer separator secret enc = unsafePerformIO $ do
    signerInstance <- constructSignerInstance signer separator secret
    enc' <- toPy enc
    unsignMethod <- getAttr signerInstance "unsign"
    callObject unsignMethod [enc'] [] >>= fromPy

signOrigWithSeparator :: Separator -> Secret -> ByteString -> ByteString
signOrigWithSeparator separator secret plain =
    signOrigWithSigner Signer separator secret plain

signOrig :: ByteString -> ByteString -> ByteString
signOrig = signOrigWithSeparator defaultSeparator

timestampSignOrigWithSeparator :: Separator
                               -> Secret
                               -> ByteString
                               -> ByteString
timestampSignOrigWithSeparator separator secret plain =
    signOrigWithSigner TimestampSigner separator secret plain

testSign :: TestTree
testSign =
    testProperty "sign s p == orignSign s p" $ \s p -> let m = sign s p in m == signOrig s p

testSignWithCustomSeparator :: TestTree
testSignWithCustomSeparator =
    testProperty
        "signWithSeparatorAndSalt timestamp defaultSeparator plain == signOrigWithSeparator defaultSeparator plain" $
        \sep secret plain ->
             signWithSeparatorAndSalt sep defaultSalt secret plain == signOrigWithSeparator sep secret plain

testSignWithTimestamp :: TestTree
testSignWithTimestamp =
  testProperty
        "signWithTimestampAndSeparator timestamp defaultSeparator plain == timestampSignOrigWithSeparator defaultSeparator plain" $
          \p ->
             let secret = "secret"
                 enc = timestampSignOrigWithSeparator defaultSeparator secret p
                 mt = extractMessageAndTimestamp defaultSeparator . unsingOrigWithSigner Signer defaultSeparator "secret" $ enc
             in Just enc ==
                (mt >>= \(_, t) -> return $ signWithTimestampAndSeparator t defaultSeparator secret p)

testUnsingTimestampped :: TestTree
testUnsingTimestampped =
    testProperty
        "unsingTimestampped enc == signWithTimestamp epoch plain" $
        (\p ->
              (unsignTimestampped (const False) "secret" $
               signWithTimestamp 1293840001 "secret" p) == Right p)

testSignFunctions :: TestTree
testSignFunctions =
    testGroup
        "ItsDangerous sign functions"
        [ testSign
        , testSignWithCustomSeparator
        , testSignWithTimestamp]

testUnsignFunctions :: TestTree
testUnsignFunctions =
    testGroup "ItsDangerous unsing functions" [testUnsingTimestampped]

tests :: TestTree
tests =
    testGroup "ItsItsDangerous" [testSignFunctions, testUnsignFunctions]

main :: IO ()
main = defaultMain tests
