module MySql.Error
       ( MySqlError (..)
       ) where

import Prelude hiding (show, unlines)

import Data.List (unlines)
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
        MySqlWrongField val expected -> unlines
            [ "MySQL error: Wrong field"
            , "  Expected: " ++ show expected
            , "  Actual: " ++ show val
            ]
        MySqlExpectedEndOfRow vals -> unlines
            [ "MySql error: Expected end of rows"
            , "  Remaining fields: " ++ show (toList vals)
            ]
        MySqlUnexpectedEndOfRow -> "MySql error: Unexpected end of row"
