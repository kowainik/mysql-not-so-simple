module MySql.Row
       ( Row (..)
       ) where

import MySql.Field (Field, FromField (..), ToField (..))

import qualified Database.MySQL.Base as SQL

{- | Something that can be represented as a row. We define this manually since
there is no @FromRow@ or @ToRow@ in @mysql-haskell@.
-}
class Row a where
    toRow   :: a -> [SQL.Param]
    fromRow :: [SQL.MySQLValue] -> Maybe a

newtype Only a = Only
    { fromOnly :: a
    } deriving (Eq, Ord, Read, Show)

instance (Field a) => Row (Only a) where
    toRow (Only a) = [toField a]
    fromRow [a] = Only <$> fromField a
    fromRow _   = Nothing

----------------------------------------------------------------------------
-- Generated instances
----------------------------------------------------------------------------

instance (Field a1, Field a2) => Row (a1,a2) where
    toRow (a1,a2) = [toField a1, toField a2]
    fromRow [a1,a2] = (,) <$> fromField a1 <*> fromField a2
    fromRow _       = Nothing

instance (Field a1, Field a2, Field a3) => Row (a1,a2,a3) where
    toRow (a1,a2,a3) = [toField a1, toField a2, toField a3]
    fromRow [a1,a2,a3] = (,,) <$> fromField a1 <*> fromField a2 <*> fromField a3
    fromRow _          = Nothing

instance (Field a1, Field a2, Field a3, Field a4) => Row (a1,a2,a3,a4) where
    toRow (a1,a2,a3,a4) = [toField a1, toField a2, toField a3, toField a4]
    fromRow [a1,a2,a3,a4] = (,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4
    fromRow _ = Nothing

instance (Field a1, Field a2, Field a3, Field a4, Field a5) => Row (a1,a2,a3,a4,a5) where
    toRow (a1,a2,a3,a4,a5) = [toField a1, toField a2, toField a3, toField a4, toField a5]
    fromRow [a1,a2,a3,a4,a5] = (,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5
    fromRow _ = Nothing

instance (Field a1, Field a2, Field a3, Field a4, Field a5, Field a6) => Row (a1,a2,a3,a4,a5,a6) where
    toRow (a1,a2,a3,a4,a5,a6) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6]
    fromRow [a1,a2,a3,a4,a5,a6] = (,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6
    fromRow _ = Nothing

instance (Field a1, Field a2, Field a3, Field a4, Field a5, Field a6, Field a7) => Row (a1,a2,a3,a4,a5,a6,a7) where
    toRow (a1,a2,a3,a4,a5,a6,a7) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7]
    fromRow [a1,a2,a3,a4,a5,a6,a7] = (,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7
    fromRow _ = Nothing

instance (Field a1, Field a2, Field a3, Field a4, Field a5, Field a6, Field a7, Field a8) => Row (a1,a2,a3,a4,a5,a6,a7,a8) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8]
    fromRow [a1,a2,a3,a4,a5,a6,a7,a8] = (,,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7 <*> fromField a8
    fromRow _ = Nothing

instance (Field a1, Field a2, Field a3, Field a4, Field a5, Field a6, Field a7, Field a8, Field a9) => Row (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9]
    fromRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = (,,,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7 <*> fromField a8 <*> fromField a9
    fromRow _ = Nothing

instance (Field a1, Field a2, Field a3, Field a4, Field a5, Field a6, Field a7, Field a8, Field a9, Field a10) => Row (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10]
    fromRow [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10] = (,,,,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7 <*> fromField a8 <*> fromField a9 <*> fromField a10
    fromRow _ = Nothing

instance (Field a1, Field a2, Field a3, Field a4, Field a5, Field a6, Field a7, Field a8, Field a9, Field a10, Field a11) => Row (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10, toField a11]
    fromRow [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11] = (,,,,,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7 <*> fromField a8 <*> fromField a9 <*> fromField a10 <*> fromField a11
    fromRow _ = Nothing

instance (Field a1, Field a2, Field a3, Field a4, Field a5, Field a6, Field a7, Field a8, Field a9, Field a10, Field a11, Field a12) => Row (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10, toField a11, toField a12]
    fromRow [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12] = (,,,,,,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7 <*> fromField a8 <*> fromField a9 <*> fromField a10 <*> fromField a11 <*> fromField a12
    fromRow _ = Nothing

instance (Field a1, Field a2, Field a3, Field a4, Field a5, Field a6, Field a7, Field a8, Field a9, Field a10, Field a11, Field a12, Field a13) => Row (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10, toField a11, toField a12, toField a13]
    fromRow [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13] = (,,,,,,,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7 <*> fromField a8 <*> fromField a9 <*> fromField a10 <*> fromField a11 <*> fromField a12 <*> fromField a13
    fromRow _ = Nothing

instance (Field a1, Field a2, Field a3, Field a4, Field a5, Field a6, Field a7, Field a8, Field a9, Field a10, Field a11, Field a12, Field a13, Field a14) => Row (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10, toField a11, toField a12, toField a13, toField a14]
    fromRow [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14] = (,,,,,,,,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7 <*> fromField a8 <*> fromField a9 <*> fromField a10 <*> fromField a11 <*> fromField a12 <*> fromField a13 <*> fromField a14
    fromRow _ = Nothing

instance (Field a1, Field a2, Field a3, Field a4, Field a5, Field a6, Field a7, Field a8, Field a9, Field a10, Field a11, Field a12, Field a13, Field a14, Field a15) => Row (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10, toField a11, toField a12, toField a13, toField a14, toField a15]
    fromRow [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15] = (,,,,,,,,,,,,,,) <$> fromField a1 <*> fromField a2 <*> fromField a3 <*> fromField a4 <*> fromField a5 <*> fromField a6 <*> fromField a7 <*> fromField a8 <*> fromField a9 <*> fromField a10 <*> fromField a11 <*> fromField a12 <*> fromField a13 <*> fromField a14 <*> fromField a15
    fromRow _ = Nothing
