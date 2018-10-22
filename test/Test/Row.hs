{-# LANGUAGE ExistentialQuantification #-}

-- | Property-based roundtrip tests for the 'FromRow' and 'ToRow' instances.

module Test.Row
       ( toRow'fromRow
       ) where

import Hedgehog (Gen, Group (..), Property, forAll, property, tripping)

import MySql (Field, FromRow (..), MySqlError (..), ToRow (..))
import MySql.Matcher (mkMatcherState, usingMatcher)
import Test.Gen (genDouble, genInt32, genMaybe, genText, genUtcTime, named)

import qualified Database.MySQL.Base as SQL
import qualified Hedgehog.Gen as Gen
import qualified Text.Show as Show


toRow'fromRow :: Group
toRow'fromRow = Group "ToRow/FromRow"
    [ genPairRow   `named` "Pair"
    , genTripleRow `named` "Triple"
    ]

data AnyField = forall a . (Field a, Eq a, Show a) => AnyField (Gen a)

instance Show AnyField where
    show _ = "AnyField"

genAnyField :: Gen AnyField
genAnyField = Gen.element
    [ AnyField genInt32
    , AnyField genDouble
    , AnyField genText
    , AnyField genMaybe
    , AnyField genUtcTime
    ]

genPairRow :: Property
genPairRow = property $ do
    AnyField gen1 <- forAll genAnyField
    AnyField gen2 <- forAll genAnyField
    a <- forAll gen1
    b <- forAll gen2
    let row = (a, b)
    tripping row toRow parseRow

genTripleRow :: Property
genTripleRow = property $ do
    AnyField gen1 <- forAll genAnyField
    AnyField gen2 <- forAll genAnyField
    AnyField gen3 <- forAll genAnyField
    a <- forAll gen1
    b <- forAll gen2
    c <- forAll gen3
    let row = (a, b, c)
    tripping row toRow parseRow

parseRow :: (FromRow a) => [SQL.Param] -> Either MySqlError a
parseRow params = mapM extractField params >>= \vals ->
    usingMatcher (mkMatcherState vals) fromRow
  where
    extractField :: SQL.Param -> Either MySqlError SQL.MySQLValue
    extractField = \case
        SQL.One val -> Right val
        SQL.Many _  -> Left MySqlUnexpectedEndOfRow  -- reusing existing error as dummy error
