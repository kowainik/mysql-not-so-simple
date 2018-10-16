{- | The Row is something that can be represented as a row. We define this
manually since there is no @FromRow@ or @ToRow@ in @mysql-haskell@.
-}

module MySql.Row
       ( ToRow (..)
       , FromRow (..)

       , Only (..)
       ) where

import MySql.Field (FromField (..), ToField (..))

import qualified Database.MySQL.Base as SQL

-- | The typeclass for rendering a collection of parameters to a SQL query.
class ToRow a where
    toRow   :: a -> [SQL.Param]

-- | The typeclass for converting a row of results returned by a SQL query into
-- a more useful Haskell representation.
class FromRow a where
    fromRow :: [SQL.MySQLValue] -> Maybe a

{- | A single-value "collection". This is useful if you need to supply a single
parameter to a SQL query, or extract a single column from a SQL result.

__Example:__

@
query [sql| SELECT x FROM scores WHERE x > ? ] (Only (42::Int))
@
-}
newtype Only a = Only
    { fromOnly :: a
    } deriving (Eq, Ord, Read, Show)

instance (ToField a) => ToRow (Only a) where
    toRow (Only a) = [toField a]

instance (FromField a) => FromRow (Only a) where
    fromRow [a] = Only <$> fromField a
    fromRow _   = Nothing

----------------------------------------------------------------------------
-- Generated instances
----------------------------------------------------------------------------
instance (ToField a1, ToField a2) => ToRow (a1,a2) where
    toRow (a1,a2) = [toField a1, toField a2]

instance (FromField a1, FromField a2) => FromRow (a1,a2) where
    fromRow [a1,a2] = (,) <$> fromField a1 <*> fromField a2
    fromRow _       = Nothing

instance (ToField a1, ToField a2, ToField a3) => ToRow (a1,a2,a3) where
    toRow (a1,a2,a3) = [toField a1, toField a2, toField a3]

instance (FromField a1, FromField a2, FromField a3) => FromRow (a1,a2,a3) where
    fromRow [a1,a2,a3] = (,,) <$> fromField a1 <*> fromField a2 <*> fromField a3
    fromRow _          = Nothing

instance (ToField a1, ToField a2, ToField a3, ToField a4) => ToRow (a1,a2,a3,a4) where
    toRow (a1,a2,a3,a4) = [toField a1, toField a2, toField a3, toField a4]

instance (FromField a1, FromField a2, FromField a3, FromField a4) => FromRow (a1,a2,a3,a4) where
    fromRow [a1,a2,a3,a4] = (,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4
    fromRow _ = Nothing

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5) => ToRow (a1,a2,a3,a4,a5) where
    toRow (a1,a2,a3,a4,a5) = [toField a1, toField a2, toField a3, toField a4, toField a5]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5) => FromRow (a1,a2,a3,a4,a5) where
    fromRow [a1,a2,a3,a4,a5] = (,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5
    fromRow _ = Nothing

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6) => ToRow (a1,a2,a3,a4,a5,a6) where
    toRow (a1,a2,a3,a4,a5,a6) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6) => FromRow (a1,a2,a3,a4,a5,a6) where
    fromRow [a1,a2,a3,a4,a5,a6] = (,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6
    fromRow _ = Nothing

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7) => ToRow (a1,a2,a3,a4,a5,a6,a7) where
    toRow (a1,a2,a3,a4,a5,a6,a7) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7) => FromRow (a1,a2,a3,a4,a5,a6,a7) where
    fromRow [a1,a2,a3,a4,a5,a6,a7] = (,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7
    fromRow _ = Nothing

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7, ToField a8) => ToRow (a1,a2,a3,a4,a5,a6,a7,a8) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7, FromField a8) => FromRow (a1,a2,a3,a4,a5,a6,a7,a8) where
    fromRow [a1,a2,a3,a4,a5,a6,a7,a8] = (,,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7 <*> fromField a8
    fromRow _ = Nothing

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7, ToField a8, ToField a9) => ToRow (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7, FromField a8, FromField a9) => FromRow (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    fromRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = (,,,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7 <*> fromField a8 <*> fromField a9
    fromRow _ = Nothing

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7, ToField a8, ToField a9, ToField a10) => ToRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7, FromField a8, FromField a9, FromField a10) => FromRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
    fromRow [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10] = (,,,,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7 <*> fromField a8 <*> fromField a9 <*> fromField a10
    fromRow _ = Nothing

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7, ToField a8, ToField a9, ToField a10, ToField a11) => ToRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10, toField a11]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7, FromField a8, FromField a9, FromField a10, FromField a11) => FromRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) where
    fromRow [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11] = (,,,,,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7 <*> fromField a8 <*> fromField a9 <*> fromField a10 <*> fromField a11
    fromRow _ = Nothing

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7, ToField a8, ToField a9, ToField a10, ToField a11, ToField a12) => ToRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10, toField a11, toField a12]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7, FromField a8, FromField a9, FromField a10, FromField a11, FromField a12) => FromRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) where
    fromRow [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12] = (,,,,,,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7 <*> fromField a8 <*> fromField a9 <*> fromField a10 <*> fromField a11 <*> fromField a12
    fromRow _ = Nothing

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7, ToField a8, ToField a9, ToField a10, ToField a11, ToField a12, ToField a13) => ToRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10, toField a11, toField a12, toField a13]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7, FromField a8, FromField a9, FromField a10, FromField a11, FromField a12, FromField a13) => FromRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) where
    fromRow [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13] = (,,,,,,,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7 <*> fromField a8 <*> fromField a9 <*> fromField a10 <*> fromField a11 <*> fromField a12 <*> fromField a13
    fromRow _ = Nothing

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7, ToField a8, ToField a9, ToField a10, ToField a11, ToField a12, ToField a13, ToField a14) => ToRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10, toField a11, toField a12, toField a13, toField a14]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7, FromField a8, FromField a9, FromField a10, FromField a11, FromField a12, FromField a13, FromField a14) => FromRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) where
    fromRow [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14] = (,,,,,,,,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7 <*> fromField a8 <*> fromField a9 <*> fromField a10 <*> fromField a11 <*> fromField a12 <*> fromField a13 <*> fromField a14
    fromRow _ = Nothing

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7, ToField a8, ToField a9, ToField a10, ToField a11, ToField a12, ToField a13, ToField a14, ToField a15) => ToRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10, toField a11, toField a12, toField a13, toField a14, toField a15]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7, FromField a8, FromField a9, FromField a10, FromField a11, FromField a12, FromField a13, FromField a14, FromField a15) => FromRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) where
    fromRow [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15] = (,,,,,,,,,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7 <*> fromField a8 <*> fromField a9 <*> fromField a10 <*> fromField a11 <*> fromField a12 <*> fromField a13 <*> fromField a14 <*> fromField a15
    fromRow _ = Nothing
