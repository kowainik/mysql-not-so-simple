module MySql.Error
       ( MySqlError (..)
       ) where


-- | MySQL error type.
data MySqlError = MySqlParseError Text
