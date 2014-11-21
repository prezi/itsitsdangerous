module Main where

import           Data.ByteString       (ByteString)
import           Data.ByteString.UTF8  (fromString)
import           Foreign.Python        (callObject, fromPy, getAttr,
                                        importModule, initialize, toPy)
import           ItsItsDangerous       (defaultSalt,
                                        defaultSeparator, rawMessage,
                                        sign, signWithSeparatorAndSalt)
import           System.IO.Unsafe      (unsafePerformIO)
import           Test.QuickCheck       (Arbitrary, arbitrary)
import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import           Test.Tasty.QuickCheck (testProperty)

instance Arbitrary ByteString where
  arbitrary   = fmap fromString arbitrary

signOrig :: ByteString -> ByteString -> ByteString
signOrig = signOrigWithSeparator defaultSeparator

signOrigWithSeparator :: ByteString -> ByteString -> ByteString -> ByteString
signOrigWithSeparator separator secret plain = unsafePerformIO $ do
  initialize False
  itsdangerous <- importModule "itsdangerous"
  signerClass <- getAttr itsdangerous "Signer"
  secret' <- toPy secret
  plain' <- toPy plain
  salt' <- toPy defaultSalt
  separator' <- toPy separator
  signerInstance <- callObject signerClass [secret', salt', separator'] []
  signMethod <- getAttr signerInstance "sign"
  callObject signMethod [plain'] [] >>= fromPy

testSign :: TestTree
testSign =
    testProperty "sign s p == orignSign s p" $ \s p -> let m = rawMessage $ sign s p in m == signOrig s p

testSignWithCustomSeparator :: TestTree
testSignWithCustomSeparator =
    testProperty
      "signWithSeparatorAndSalt sep defaultSeparator s p == signOrigWithSeparator sep s p" $
        \sep s p -> let m = rawMessage $ signWithSeparatorAndSalt sep defaultSalt s p in m == signOrigWithSeparator sep s p


testItsDangerousAPI :: TestTree
testItsDangerousAPI = testGroup "ItsDangerous API" [ testSign, testSignWithCustomSeparator ]

tests :: TestTree
tests = testGroup "ItsItsDangerous" [ testSign, testSignWithCustomSeparator ]

main :: IO ()
main = defaultMain tests
