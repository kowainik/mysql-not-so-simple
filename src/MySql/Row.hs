{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}

{- | The Row is something that can be represented as a row. We define this
manually since there is no @FromRow@ or @ToRow@ in @mysql-haskell@.
-}

module MySql.Row
       ( ToRow (..)
       , FromRow (..)

       , Only (..)
       ) where

import GHC.Generics ((:*:) (..), Generic (Rep), K1 (..), M1 (..))

import MySql.Field (FromField (..), ToField (..))
import MySql.Matcher (Matcher, field)

import qualified Database.MySQL.Base as SQL
import qualified GHC.Generics as Generic

----------------------------------------------------------------------------
-- ToRow
----------------------------------------------------------------------------

-- | The typeclass for rendering a collection of parameters to a SQL query.
class ToRow a where
    toRow   :: a -> [SQL.Param]

    default toRow :: (Generic a, GenericToRow (Rep a)) => a -> [SQL.Param]
    toRow = genericToRow . Generic.from

-- Type class for default implementation of ToRow using generics
class GenericToRow (f :: k -> Type) where
    genericToRow :: f p -> [SQL.Param]

instance GenericToRow f => GenericToRow (M1 i m f) where
    genericToRow (M1 f) = genericToRow f

instance (GenericToRow f, GenericToRow g) => GenericToRow (f :*: g) where
    genericToRow (f :*: g) = genericToRow f ++ genericToRow g

instance ToField a => GenericToRow (K1 i a) where
    genericToRow (K1 a) = [toField a]

----------------------------------------------------------------------------
-- FromRow
----------------------------------------------------------------------------

{- | The typeclass for converting a row of results returned by a SQL query into
a more useful Haskell representation. Use 'MySql.Matcher.field' function to
implement instances of this typeclass.
-}
class FromRow a where
    fromRow :: Matcher a

    default fromRow :: (Generic a, GenericFromRow (Rep a)) => Matcher a
    fromRow = Generic.to <$> genericFromRow

-- Type class for default implementation of ToRow using generics
class GenericFromRow (f :: k -> Type) where
    genericFromRow :: Matcher (f p)

instance GenericFromRow f => GenericFromRow (M1 i m f) where
    genericFromRow = M1 <$> genericFromRow

instance (GenericFromRow f, GenericFromRow g) => GenericFromRow (f :*: g) where
    genericFromRow = liftA2 (:*:) genericFromRow genericFromRow

instance FromField a => GenericFromRow (K1 i a) where
    genericFromRow = K1 <$> field

----------------------------------------------------------------------------
-- Manual instances
----------------------------------------------------------------------------

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
    fromRow = Only <$> field

instance ToField a => ToRow [a] where
    toRow = map toField

instance ToField a => ToRow (NonEmpty a) where
    toRow = map toField . toList

----------------------------------------------------------------------------
-- Generated instances
----------------------------------------------------------------------------

instance (ToField a1, ToField a2) => ToRow (a1,a2) where
    toRow (a1,a2) = [toField a1, toField a2]

instance (FromField a1, FromField a2) => FromRow (a1,a2) where
    fromRow = (,) <$> field <*> field

instance (ToField a1, ToField a2, ToField a3) => ToRow (a1,a2,a3) where
    toRow (a1,a2,a3) = [toField a1, toField a2, toField a3]

instance (FromField a1, FromField a2, FromField a3) => FromRow (a1,a2,a3) where
    fromRow = (,,) <$> field <*> field <*> field

instance (ToField a1, ToField a2, ToField a3, ToField a4) => ToRow (a1,a2,a3,a4) where
    toRow (a1,a2,a3,a4) = [toField a1, toField a2, toField a3, toField a4]

instance (FromField a1, FromField a2, FromField a3, FromField a4) => FromRow (a1,a2,a3,a4) where
    fromRow = (,,,) <$> field <*> field <*> field <*> field

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5) => ToRow (a1,a2,a3,a4,a5) where
    toRow (a1,a2,a3,a4,a5) = [toField a1, toField a2, toField a3, toField a4, toField a5]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5) => FromRow (a1,a2,a3,a4,a5) where
    fromRow = (,,,,) <$> field <*> field <*> field <*> field <*> field

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6) => ToRow (a1,a2,a3,a4,a5,a6) where
    toRow (a1,a2,a3,a4,a5,a6) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6) => FromRow (a1,a2,a3,a4,a5,a6) where
    fromRow = (,,,,,) <$> field <*> field <*> field <*> field <*> field <*> field

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7) => ToRow (a1,a2,a3,a4,a5,a6,a7) where
    toRow (a1,a2,a3,a4,a5,a6,a7) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7) => FromRow (a1,a2,a3,a4,a5,a6,a7) where
    fromRow = (,,,,,,) <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7, ToField a8) => ToRow (a1,a2,a3,a4,a5,a6,a7,a8) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7, FromField a8) => FromRow (a1,a2,a3,a4,a5,a6,a7,a8) where
    fromRow = (,,,,,,,) <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7, ToField a8, ToField a9) => ToRow (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7, FromField a8, FromField a9) => FromRow (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    fromRow = (,,,,,,,,) <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7, ToField a8, ToField a9, ToField a10) => ToRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7, FromField a8, FromField a9, FromField a10) => FromRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
    fromRow = (,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7, ToField a8, ToField a9, ToField a10, ToField a11) => ToRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10, toField a11]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7, FromField a8, FromField a9, FromField a10, FromField a11) => FromRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) where
    fromRow = (,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7, ToField a8, ToField a9, ToField a10, ToField a11, ToField a12) => ToRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10, toField a11, toField a12]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7, FromField a8, FromField a9, FromField a10, FromField a11, FromField a12) => FromRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) where
    fromRow = (,,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7, ToField a8, ToField a9, ToField a10, ToField a11, ToField a12, ToField a13) => ToRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10, toField a11, toField a12, toField a13]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7, FromField a8, FromField a9, FromField a10, FromField a11, FromField a12, FromField a13) => FromRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) where
    fromRow = (,,,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7, ToField a8, ToField a9, ToField a10, ToField a11, ToField a12, ToField a13, ToField a14) => ToRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10, toField a11, toField a12, toField a13, toField a14]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7, FromField a8, FromField a9, FromField a10, FromField a11, FromField a12, FromField a13, FromField a14) => FromRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) where
    fromRow = (,,,,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance (ToField a1, ToField a2, ToField a3, ToField a4, ToField a5, ToField a6, ToField a7, ToField a8, ToField a9, ToField a10, ToField a11, ToField a12, ToField a13, ToField a14, ToField a15) => ToRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) where
    toRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) = [toField a1, toField a2, toField a3, toField a4, toField a5, toField a6, toField a7, toField a8, toField a9, toField a10, toField a11, toField a12, toField a13, toField a14, toField a15]

instance (FromField a1, FromField a2, FromField a3, FromField a4, FromField a5, FromField a6, FromField a7, FromField a8, FromField a9, FromField a10, FromField a11, FromField a12, FromField a13, FromField a14, FromField a15) => FromRow (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) where
    fromRow = (,,,,,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
