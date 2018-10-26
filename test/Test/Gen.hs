-- | Common generators

module Test.Gen
       ( named

       , genInt
       , genInt32
       , genWord32
       , genWord64
       , genDouble
       , genText
       , genLText
       , genByteString
       , genLByteString
       , genBool
       , genMaybe
       , genUtcTime
       ) where

import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..))
import Hedgehog (Gen)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


named :: b -> a -> (a, b)
named b a = (a, b)

genInt :: Gen Int
genInt = Gen.int Range.constantBounded

genInt32 :: Gen Int32
genInt32 = Gen.int32 Range.constantBounded

genWord32 :: Gen Word32
genWord32 = Gen.word32 Range.constantBounded

genWord64 :: Gen Word64
genWord64 = Gen.word64 Range.constantBounded

genDouble :: Gen Double
genDouble = Gen.double $ Range.linearFrac (-1e6) 1e6

genText :: Gen Text
genText = Gen.text (Range.constant 0 1000) Gen.unicode

genLText :: Gen LText
genLText = toLazy <$> genText

genByteString :: Gen ByteString
genByteString = Gen.bytes (Range.constant 0 1000)

genLByteString :: Gen LByteString
genLByteString = toLazy <$> genByteString

genBool :: Gen Bool
genBool = Gen.bool

genMaybe :: Gen (Maybe Int32)
genMaybe = Gen.maybe genInt32

-- shamelessly copy-pasted
genUtcTime :: Gen UTCTime
genUtcTime = do
    day <- Gen.int (Range.linear 50000 60000)
    seconds <- Gen.int (Range.linear 0 86400)
    return $ UTCTime (ModifiedJulianDay $ fromIntegral day) (realToFrac seconds)
