{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE StandaloneDeriving #-}

-- | Property-based roundtrip tests for the 'FromField' and 'ToField' instances.

module Test.Field
       ( toField'fromField
       ) where

import Hedgehog (Gen, Group (..), Property, forAll, property, tripping)

import MySql (Field, FromField (..), MySqlError (..), ToField (..))
import Test.Gen (genDouble, genInt32, genMaybe, genText, genUtcTime, named)

import qualified Database.MySQL.Base as SQL


toField'fromField :: Group
toField'fromField = Group "ToField/FromField"
    [ fieldProperty genInt32   `named` "Int32"
    , fieldProperty genDouble  `named` "Double"
    , fieldProperty genText    `named` "Text"
    , fieldProperty genMaybe   `named` "Maybe Int32"
    , fieldProperty genUtcTime `named` "UtcTime"
    ]

fieldProperty :: forall a . (Field a, Eq a, Show a) => Gen a -> Property
fieldProperty genField = property $ do
    field <- forAll genField
    tripping field toField extractField
  where
    extractField :: SQL.Param -> Either MySqlError a
    extractField = \case
        SQL.One val -> fromField val
        SQL.Many _  -> Left MySqlUnexpectedEndOfRow  -- reusing existing error as dummy error

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

deriving instance Show SQL.Param
