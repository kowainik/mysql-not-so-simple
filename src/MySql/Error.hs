{-# LANGUAGE ConstraintKinds #-}

module MySql.Error
       ( MySqlError (..)
       , WithError
       ) where

import Prelude hiding (lines, show, unlines)

import Control.Monad.Except (MonadError)
import Data.List (lines, unlines)
import Text.Show (show)

import MySql.Named.Core (Name)

import qualified Database.MySQL.Base as SQL


-- | MySQL error type.
data MySqlError
    -- | Error when parsing MySQLValue to Haskell type
    = MySqlWrongField SQL.MySQLValue Text
    -- | MySQL error on given column
    | MySqlWrongColumn Int MySqlError
    -- | More columns were returned by the SQL query than expected
    | MySqlExpectedEndOfRow (NonEmpty SQL.MySQLValue)
    -- | Less columns were returned by the SQL query than expected
    | MySqlUnexpectedEndOfRow
    -- | Named param is not specified
    | MySqlNamedError Name
    -- | Query has no names inside but was called with named functions
    | MySqlNoNames SQL.Query
    -- | Query contains empty name
    | MySqlEmptyName SQL.Query
    deriving (Eq)

instance Show MySqlError where
    show = \case
        MySqlWrongField val expected -> unlines
            [ "MySQL error: Wrong field"
            , "  Expected: " ++ toString expected
            , "  Actual: " ++ show val
            ]
        MySqlWrongColumn pos err -> unlines $
            ( "MySQL error: the following error at column " ++ show pos )
          : map ("  " ++) (lines $ show err)
        MySqlExpectedEndOfRow vals -> unlines
            [ "MySql error: Expected end of rows"
            , "  Remaining fields: " ++ show (toList vals)
            ]
        MySqlUnexpectedEndOfRow -> "MySql error: Unexpected end of row"
        MySqlNamedError n -> "MySql error: Named param :" ++ show n ++ " is not specified"
        MySqlNoNames (SQL.Query q) ->
            "MySql error: Query has no names but was called with named functions: " ++ decodeUtf8 q
        MySqlEmptyName (SQL.Query q) ->
            "MySql error: Query contains empty name: " ++ decodeUtf8 q


-- | Type alias for 'MySqlError'.
type WithError = MonadError MySqlError
