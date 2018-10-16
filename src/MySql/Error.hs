module MySql.Error
       ( MySqlError (..)
       ) where

import qualified Database.MySQL.Base as SQL


-- | MySQL error type.
data MySqlError
    = MySqlWrongField SQL.MySQLValue Text
    | MySqlExpectedEndOfRow (NonEmpty SQL.MySQLValue)
    | MySqlUnexpectedEndOfRow
    deriving (Show)
