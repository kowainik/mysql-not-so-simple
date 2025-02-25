-- | Property-based roundtrip tests for the 'FromField' and 'ToField' instances.

module Test.Field
       ( toField'fromField
       ) where

import Hedgehog (Gen, Group (..), Property, forAll, property, tripping)

import MySql (Field, FromField (..), MySqlError (..), ToField (..))
import Test.Gen (genBool, genByteString, genDouble, genInt, genInt32, genLByteString, genLText,
                 genMaybe, genText, genUtcTime, genWord32, genWord64, named)

import qualified Database.MySQL.Base as SQL


toField'fromField :: Group
toField'fromField = Group "ToField/FromField"
    [ fieldProperty genInt32       `named` "Int32"
    , fieldProperty genInt         `named` "Int"
    , fieldProperty genWord32      `named` "Word32"
    , fieldProperty genWord64      `named` "Word64"
    , fieldProperty genDouble      `named` "Double"
    , fieldProperty genBool        `named` "Bool"
    , fieldProperty genText        `named` "Text"
    , fieldProperty genLText       `named` "LText"
    , fieldProperty genByteString  `named` "ByteString"
    , fieldProperty genLByteString `named` "LByteString"
    , fieldProperty genMaybe       `named` "Maybe Int32"
    , fieldProperty genUtcTime     `named` "UtcTime"
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
