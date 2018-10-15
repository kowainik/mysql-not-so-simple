module MySql.Row
       ( Row (..)
       ) where

import MySql.Field (Field (..))

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

instance (Field a, Field b) => Row (a,b) where
    toRow (a,b) = [toField a, toField b]
    fromRow [a,b] = (,) <$> fromField a <*> fromField b
    fromRow _     = Nothing

instance (Field a, Field b, Field c) => Row (a,b,c) where
    toRow (a,b,c) = [toField a, toField b, toField c]
    fromRow [a,b,c] = (,,) <$> fromField a <*> fromField b <*> fromField c
    fromRow _       = Nothing

instance (Field a, Field b, Field c, Field d) => Row (a,b,c,d) where
    toRow (a,b,c,d) = [toField a, toField b, toField c, toField d]
    fromRow [a,b,c,d] = (,,,) <$> fromField a <*> fromField b <*> fromField c <*> fromField d
    fromRow _         = Nothing

instance (Field a, Field b, Field c, Field d, Field e) => Row (a,b,c,d,e) where
    toRow (a,b,c,d,e) = [toField a, toField b, toField c, toField d, toField e]
    fromRow [a,b,c,d,e] = (,,,,) <$> fromField a <*> fromField b <*> fromField c <*> fromField d <*> fromField e
    fromRow _           = Nothing

instance (Field a, Field b, Field c, Field d, Field e, Field f) => Row (a,b,c,d,e,f) where
    toRow (a,b,c,d,e,f) = [toField a, toField b, toField c, toField d, toField e, toField f]
    fromRow [a,b,c,d,e,f] = (,,,,,) <$> fromField a <*> fromField b <*> fromField c <*> fromField d <*> fromField e <*> fromField f
    fromRow _ = Nothing

instance (Field a, Field b, Field c, Field d, Field e, Field f, Field g) => Row (a,b,c,d,e,f,g) where
    toRow (a,b,c,d,e,f,g) = [toField a, toField b, toField c, toField d, toField e, toField f, toField g]
    fromRow [a,b,c,d,e,f,g] = (,,,,,,) <$> fromField a <*> fromField b <*> fromField c <*> fromField d <*> fromField e <*> fromField f <*> fromField g
    fromRow _ = Nothing

instance (Field a, Field b, Field c, Field d, Field e, Field f, Field g, Field h) => Row (a,b,c,d,e,f,g,h) where
    toRow (a,b,c,d,e,f,g,h) = [toField a, toField b, toField c, toField d, toField e, toField f, toField g, toField h]
    fromRow [a,b,c,d,e,f,g,h] = (,,,,,,,) <$> fromField a <*> fromField b <*> fromField c <*> fromField d <*> fromField e <*> fromField f <*> fromField g <*> fromField h
    fromRow _ = Nothing

instance (Field a, Field b, Field c, Field d, Field e, Field f, Field g, Field h, Field i) => Row (a,b,c,d,e,f,g,h,i) where
    toRow (a,b,c,d,e,f,g,h,i) = [toField a, toField b, toField c, toField d, toField e, toField f, toField g, toField h, toField i]
    fromRow [a,b,c,d,e,f,g,h,i] = (,,,,,,,,) <$> fromField a <*> fromField b <*> fromField c <*> fromField d <*> fromField e <*> fromField f <*> fromField g <*> fromField h <*> fromField i
    fromRow _ = Nothing

instance (Field a, Field b, Field c, Field d, Field e, Field f, Field g, Field h, Field i, Field j) => Row (a,b,c,d,e,f,g,h,i,j) where
    toRow (a,b,c,d,e,f,g,h,i,j) = [toField a, toField b, toField c, toField d, toField e, toField f, toField g, toField h, toField i, toField j]
    fromRow [a,b,c,d,e,f,g,h,i,j] = (,,,,,,,,,) <$> fromField a <*> fromField b <*> fromField c <*> fromField d <*> fromField e <*> fromField f <*> fromField g <*> fromField h <*> fromField i <*> fromField j
    fromRow _ = Nothing

instance (Field a, Field b, Field c, Field d, Field e, Field f, Field g, Field h, Field i, Field j, Field k) => Row (a,b,c,d,e,f,g,h,i,j,k) where
    toRow (a,b,c,d,e,f,g,h,i,j,k) = [toField a, toField b, toField c, toField d, toField e, toField f, toField g, toField h, toField i, toField j, toField k]
    fromRow [a,b,c,d,e,f,g,h,i,j,k] = (,,,,,,,,,,) <$> fromField a <*> fromField b <*> fromField c <*> fromField d <*> fromField e <*> fromField f <*> fromField g <*> fromField h <*> fromField i <*> fromField j <*> fromField k
    fromRow _ = Nothing

instance (Field a, Field b, Field c, Field d, Field e, Field f, Field g, Field h, Field i, Field j, Field k, Field l) => Row (a,b,c,d,e,f,g,h,i,j,k,l) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l) = [toField a, toField b, toField c, toField d, toField e, toField f, toField g, toField h, toField i, toField j, toField k, toField l]
    fromRow [a,b,c,d,e,f,g,h,i,j,k,l] = (,,,,,,,,,,,) <$> fromField a <*> fromField b <*> fromField c <*> fromField d <*> fromField e <*> fromField f <*> fromField g <*> fromField h <*> fromField i <*> fromField j <*> fromField k <*> fromField l
    fromRow _ = Nothing

instance (Field a, Field b, Field c, Field d, Field e, Field f, Field g, Field h, Field i, Field j, Field k, Field l, Field m) => Row (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m) = [toField a, toField b, toField c, toField d, toField e, toField f, toField g, toField h, toField i, toField j, toField k, toField l, toField m]
    fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m] = (,,,,,,,,,,,,) <$> fromField a <*> fromField b <*> fromField c <*> fromField d <*> fromField e <*> fromField f <*> fromField g <*> fromField h <*> fromField i <*> fromField j <*> fromField k <*> fromField l <*> fromField m
    fromRow _ = Nothing

instance (Field a, Field b, Field c, Field d, Field e, Field f, Field g, Field h, Field i, Field j, Field k, Field l, Field m, Field n) => Row (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = [toField a, toField b, toField c, toField d, toField e, toField f, toField g, toField h, toField i, toField j, toField k, toField l, toField m, toField n]
    fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m,n] = (,,,,,,,,,,,,,) <$> fromField a <*> fromField b <*> fromField c <*> fromField d <*> fromField e <*> fromField f <*> fromField g <*> fromField h <*> fromField i <*> fromField j <*> fromField k <*> fromField l <*> fromField m <*> fromField n
    fromRow _ = Nothing

instance (Field a, Field b, Field c, Field d, Field e, Field f, Field g, Field h, Field i, Field j, Field k, Field l, Field m, Field n, Field o) => Row (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = [toField a, toField b, toField c, toField d, toField e, toField f, toField g, toField h, toField i, toField j, toField k, toField l, toField m, toField n, toField o]
    fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o] = (,,,,,,,,,,,,,,) <$> fromField a <*> fromField b <*> fromField c <*> fromField d <*> fromField e <*> fromField f <*> fromField g <*> fromField h <*> fromField i <*> fromField j <*> fromField k <*> fromField l <*> fromField m <*> fromField n <*> fromField o
    fromRow _ = Nothing

instance (Field a, Field b, Field c, Field d, Field e, Field f, Field g, Field h, Field i, Field j, Field k, Field l, Field m, Field n, Field o, Field p) => Row (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) = [toField a, toField b, toField c, toField d, toField e, toField f, toField g, toField h, toField i, toField j, toField k, toField l, toField m, toField n, toField o, toField p]
    fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] = (,,,,,,,,,,,,,,,) <$> fromField a <*> fromField b <*> fromField c <*> fromField d <*> fromField e <*> fromField f <*> fromField g <*> fromField h <*> fromField i <*> fromField j <*> fromField k <*> fromField l <*> fromField m <*> fromField n <*> fromField o <*> fromField p
    fromRow _ = Nothing
