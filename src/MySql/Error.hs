module MySql.Error
       ( MySqlError (..)
       ) where

import qualified Database.MySQL.Base as SQL


-- | MySQL error type.
data MySqlError
    = MySqlWrongField SQL.MySQLValue
    | MySqlExpectedEndOfRow (NonEmpty SQL.MySQLValue)
    | MySqlUnexpectedEndOfRow
    deriving (Show)
