{-# LANGUAGE ConstraintKinds #-}

{- | The field is something that can be represented as a column.

We define these type classes manually since there is no 'FromField'
or 'ToField' in @mysql-haskell@.
-}

module MySql.Field
       ( ToField (..)
       , FromField (..)
       , Field
       ) where

import qualified Database.MySQL.Base as SQL


-- | To render the data type to a SQL query.
class ToField a where
  toField   :: a -> SQL.Param

-- | To convert a single value in a row returned by a SQL query into a data type.
class FromField a where
  fromField :: SQL.MySQLValue -> Maybe a

-- | Type alias constraint for 'ToField' and 'FromField' type classes.
type Field a = (ToField a, FromField a)

-- Define column for built in types.
-- Refer to http://hackage.haskell.org/package/mysql-haskell-0.8.3.0/docs/Database-MySQL-Protocol-MySQLValue.html

instance ToField Text where
    toField = SQL.One . SQL.MySQLText

instance FromField Text where
    fromField (SQL.MySQLText x) = Just x
    fromField _                 = Nothing

instance ToField Int32 where
    toField = SQL.One . SQL.MySQLInt32

instance FromField Int32 where
    fromField (SQL.MySQLInt32 x) = Just x
    fromField _                  = Nothing

instance ToField Double where
    toField = SQL.One . SQL.MySQLDouble

instance FromField Double where
    fromField (SQL.MySQLDouble x) = Just x
    fromField _                   = Nothing

-- TODO: The `fromC` definition here makes no sense?
instance ToField a => ToField [a] where
    toField x = SQL.Many $ foldl' folder [] x
      where
        folder acc a = case toField a of
            SQL.One a' -> a':acc
            _          -> acc

instance FromField a => FromField [a] where
    fromField x = pure <$> fromField x
