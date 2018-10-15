module MySql.Field
       ( Field (..)
       ) where

import qualified Database.MySQL.Base as SQL

{- | Something that can be represented as a column. We define this manually
since there is no `FromField` or `ToField` in `mysql-haskell`.
-}
class Field a where
  toField   :: a -> SQL.Param
  fromField :: SQL.MySQLValue -> Maybe a

-- Define column for built in types.
-- Refer to http://hackage.haskell.org/package/mysql-haskell-0.8.3.0/docs/Database-MySQL-Protocol-MySQLValue.html

instance Field Text where
    toField = SQL.One . SQL.MySQLText
    fromField (SQL.MySQLText x) = Just x
    fromField _                 = Nothing

instance Field Int32 where
    toField = SQL.One . SQL.MySQLInt32
    fromField (SQL.MySQLInt32 x) = Just x
    fromField _                  = Nothing

instance Field Double where
    toField = SQL.One . SQL.MySQLDouble
    fromField (SQL.MySQLDouble x) = Just x
    fromField _                   = Nothing

-- TODO: The `fromC` definition here makes no sense?
instance Field a => Field [a] where
  fromField x = pure <$> fromField x
  toField x = SQL.Many $ foldl' folder [] x
    where
      folder acc a = case toField a of
          SQL.One a' -> a':acc
          _          -> acc
