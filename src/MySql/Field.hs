{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- | The field is something that can be represented as a column.

We define these type classes manually since there is no 'FromField'
or 'ToField' in @mysql-haskell@.
-}

module MySql.Field
       ( ToField (..)
       , FromField (..)
       , Field

       , LastId (..)
       , Lots (..)
       ) where

import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (localTimeToUTC, utc, utcToLocalTime)

import MySql.Error (MySqlError (..))

import qualified Database.MySQL.Base as SQL


-- | To render the data type to a SQL query.
class ToField a where
  toField :: a -> SQL.Param

-- | To convert a single value in a row returned by a SQL query into a data type.
class FromField a where
  fromField :: SQL.MySQLValue -> Either MySqlError a

-- | Type alias constraint for 'ToField' and 'FromField' type classes.
type Field a = (ToField a, FromField a)

-- Define column for built in types.
-- Refer to http://hackage.haskell.org/package/mysql-haskell-0.8.3.0/docs/Database-MySQL-Protocol-MySQLValue.html

instance ToField SQL.Param where
    toField = id

instance FromField SQL.MySQLValue where
    fromField = pure

instance ToField Text where
    toField = SQL.One . SQL.MySQLText

instance FromField Text where
    fromField (SQL.MySQLText x) = Right x
    fromField x                 = Left $ MySqlWrongField x "Text"

instance ToField LText where
    toField = toField . fromLazy

instance FromField LText where
    fromField (SQL.MySQLText x) = Right $ toLazy x
    fromField x                 = Left $ MySqlWrongField x "LText"

instance ToField ByteString where
    toField = SQL.One . SQL.MySQLBytes

instance FromField ByteString where
    fromField (SQL.MySQLBytes x) = Right x
    fromField x                  = Left $ MySqlWrongField x "ByteString"

instance ToField LByteString where
    toField = toField . fromLazy

instance FromField LByteString where
    fromField (SQL.MySQLBytes x) = Right $ toLazy x
    fromField x                  = Left $ MySqlWrongField x "LByteString"

instance ToField Int where
    toField = SQL.One . SQL.MySQLInt64 . fromIntegral @Int @Int64

instance FromField Int where
    fromField (SQL.MySQLInt64 x) = Right $ fromIntegral @Int64 @Int x
    fromField x                  = Left $ MySqlWrongField x "Int"

instance ToField Bool where
    toField True  = SQL.One $ SQL.MySQLInt64 1
    toField False = SQL.One $ SQL.MySQLInt64 0

instance FromField Bool where
    fromField (SQL.MySQLInt64 x) = Right $ if x == 0 then False else True
    fromField x                  = Left $ MySqlWrongField x "Bool"

instance ToField Int32 where
    toField = SQL.One . SQL.MySQLInt32

instance FromField Int32 where
    fromField (SQL.MySQLInt32 x) = Right x
    fromField x                  = Left $ MySqlWrongField x "Int32"

instance ToField Word32 where
    toField = SQL.One . SQL.MySQLInt32U

instance FromField Word32 where
    fromField (SQL.MySQLInt32U x) = Right x
    fromField x                   = Left $ MySqlWrongField x "Word32"

instance ToField Word64 where
    toField = SQL.One . SQL.MySQLInt64U

instance FromField Word64 where
    fromField (SQL.MySQLInt64U x) = Right x
    fromField x                   = Left $ MySqlWrongField x "Word64"

instance ToField Double where
    toField = SQL.One . SQL.MySQLDouble

instance FromField Double where
    fromField (SQL.MySQLDouble x) = Right x
    fromField x                   = Left $ MySqlWrongField x "Double"


instance (ToField a) => ToField (Maybe a) where
    toField Nothing  = SQL.One SQL.MySQLNull
    toField (Just a) = toField a

instance (FromField a) => FromField (Maybe a) where
    fromField SQL.MySQLNull = pure Nothing
    fromField f             = Just <$> fromField @a f

instance ToField UTCTime where
    toField = SQL.One . SQL.MySQLTimeStamp . utcToLocalTime utc

instance FromField UTCTime where
    fromField (SQL.MySQLTimeStamp localTime) = Right $ localTimeToUTC utc localTime
    fromField x                              = Left $ MySqlWrongField x "TimeStamp"

-- | This data type is used in queries that return id of last inserted row.
newtype LastId = LastId
    { unLastId :: Int
    } deriving newtype (Eq, Ord, Show, ToField)

{- | This data type is supposed to be used to substitue multiple arguments. Like this:

@
[sql| SELECT * FROM test WHERE _id IN (?, "888") |] ('Lots' ["hello", "world"])
@
-}
newtype Lots a = Lots
    { unLots :: [a]
    }

instance ToField a => ToField (Lots a) where
    toField = SQL.Many . foldr combine [] . unLots
      where
        combine :: a -> [SQL.MySQLValue] -> [SQL.MySQLValue]
        combine a vals = case toField a of
            SQL.One val -> val : vals
            _           -> vals
