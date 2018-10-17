module MySql.Error
       ( MySqlError (..)
       ) where

import Prelude hiding (show)

import Text.Show (show)

import qualified Database.MySQL.Base as SQL


-- | MySQL error type.
data MySqlError
    = MySqlWrongField SQL.MySQLValue Text
    | MySqlExpectedEndOfRow (NonEmpty SQL.MySQLValue)
    | MySqlUnexpectedEndOfRow
    deriving (Eq)

instance Show MySqlError where
    show = \case
        (MySqlWrongField val expected) -> "MySQL error: Wrong field\n  Expected: "
            ++ show expected ++ "\n  Actual: " ++ show val
        (MySqlExpectedEndOfRow vals) -> "MySql error: Expected end of rows\n  Remaining fields: "
            ++ show (toList vals)
        MySqlUnexpectedEndOfRow -> "MySql error: Unexpected end of row"
