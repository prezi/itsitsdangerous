{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Foreign.Python
       (callObject, fromPy, getAttr, importModule, initialize, toPy,
        PyObject)
import ItsItsDangerous
       (defaultSalt, Separator, Secret, defaultSeparator, rawMessage,
        sign, signWithSeparatorAndSalt, extractTimestamp, base64decode,
        base64encode, signWithTimestampAndSeparator, attachTimestamp,
        int2bytes)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

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

signOrigWithSigner :: OrigSigner -> Separator -> Secret -> ByteString -> ByteString
signOrigWithSigner signer separator secret plain = unsafePerformIO $ do
    signerInstance <- constructSignerInstance signer separator secret
    plain' <- toPy plain
    signMethod <- getAttr signerInstance "sign"
    callObject signMethod [plain'] [] >>= fromPy

unsingOrigWithSigner :: OrigSigner -> Separator -> Secret -> ByteString -> ByteString
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

timestampSignOrigWithSeparator :: Separator -> Secret -> ByteString -> ByteString
timestampSignOrigWithSeparator separator secret plain =
    signOrigWithSigner TimestampSigner separator secret plain

testSign :: TestTree
testSign =
    testProperty "sign s p == orignSign s p" $ \s p -> let m = rawMessage $ sign s p in m == signOrig s p

testSignWithCustomSeparator :: TestTree
testSignWithCustomSeparator =
    testProperty
        "signWithSeparatorAndSalt timestamp defaultSeparator plain == signOrigWithSeparator defaultSeparator plain" $
        \sep secret plain ->
             let m =
                     rawMessage $
                     signWithSeparatorAndSalt sep defaultSalt secret plain
             in m ==
                signOrigWithSeparator sep secret plain

testSignWithTimestamp :: TestTree
testSignWithTimestamp =
  testProperty
        "signWithTimestampAndSeparator timestamp defaultSeparator plain == timestampSignOrigWithSeparator defaultSeparator plain" $
          \p ->
             let secret = "secret"
                 sep = "/"
                 enc = timestampSignOrigWithSeparator sep secret p
                 mt = extractTimestamp sep p $ unsingOrigWithSigner Signer sep "secret" $ enc
             in Just enc ==
                (mt >>= \t -> return . rawMessage $ signWithTimestampAndSeparator t sep secret p)

testSignFunctions :: TestTree
testSignFunctions = testGroup "ItsDangerous sign functions" [ testSign, testSignWithCustomSeparator ]

tests :: TestTree
tests = testGroup "ItsItsDangerous" [ testSignFunctions, testSignWithCustomSeparator, testSignWithTimestamp ]

main :: IO ()
main = defaultMain tests
