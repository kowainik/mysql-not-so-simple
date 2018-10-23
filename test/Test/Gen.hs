{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE StandaloneDeriving #-}

-- | Common generators

module Test.Gen
       ( named

       , genInt32
       , genDouble
       , genText
       , genMaybe
       , genUtcTime
       ) where

import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..))
import Hedgehog (Gen)

import qualified Database.MySQL.Base as SQL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


named :: b -> a -> (a, b)
named b a = (a, b)

genInt32 :: Gen Int32
genInt32 = Gen.int32 Range.constantBounded

genDouble :: Gen Double
genDouble = Gen.double $ Range.linearFrac (-1e6) 1e6

genText :: Gen Text
genText = Gen.text (Range.constant 0 1000) Gen.alphaNum  -- TODO: somehow doesn't work with unicode :shrug:

genMaybe :: Gen (Maybe Int32)
genMaybe = Gen.maybe genInt32

-- shamelessly copy-pasted
genUtcTime :: Gen UTCTime
genUtcTime = do
    day <- Gen.int (Range.linear 50000 60000)
    seconds <- Gen.int (Range.linear 0 86400)
    return $ UTCTime (ModifiedJulianDay $ fromIntegral day) (realToFrac seconds)

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

deriving instance Show SQL.Param
