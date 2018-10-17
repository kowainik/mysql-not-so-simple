{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE StandaloneDeriving #-}

-- | Property-based roundtrip tests for the 'FromField' and 'ToField' instances.

module Test.Field
       ( toField'fromField
       ) where

import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..))
import Hedgehog (Gen, Group (..), Property, forAll, property, tripping)

import MySql (Field, FromField (..), MySqlError (..), ToField (..))

import qualified Database.MySQL.Base as SQL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


toField'fromField :: Group
toField'fromField = Group "ToField/FromField"
    [ fieldProperty genInt32   `named` "Int32"
    , fieldProperty genDouble  `named` "Double"
    , fieldProperty genText    `named` "Text"
    , fieldProperty genMaybe   `named` "Maybe Int32"
    , fieldProperty genUtcTime `named` "UtcTime"
    ]
  where
    named :: b -> a -> (a, b)
    named b a = (a, b)

fieldProperty :: forall a . (Field a, Eq a, Show a) => Gen a -> Property
fieldProperty genField = property $ do
    field <- forAll genField
    tripping field toField extractField
  where
    extractField :: SQL.Param -> Either MySqlError a
    extractField = \case
        SQL.One val -> fromField val
        SQL.Many _  -> Left MySqlUnexpectedEndOfRow  -- reusing existing error as dummy error

genInt32 :: Gen Int32
genInt32 = Gen.int32 Range.constantBounded

genDouble :: Gen Double
genDouble = Gen.double $ Range.linearFrac (-1e6) 1e6

genText :: Gen Text
genText = Gen.text (Range.constant 0 1000) Gen.unicode

genMaybe :: Gen (Maybe Int32)
genMaybe = Gen.maybe genInt32

-- shamelessly copy-pasted
genUtcTime :: Gen UTCTime
genUtcTime = do
    day <- Gen.int (Range.linear 5000 10000)
    seconds <- Gen.int (Range.linear 0 86400)
    return $ UTCTime (ModifiedJulianDay $ fromIntegral day) (realToFrac seconds)

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

deriving instance Show SQL.Param
