{-# LANGUAGE DerivingStrategies #-}

module MySql.Named
       ( NamedParam (..)
       , Name (..)

       , extractNames
       ) where

import Data.Char (isAlphaNum)

import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Database.MySQL.Base as SQL

newtype Name = Name
    { unName :: Text
    } deriving newtype (Eq, Ord)

-- please, send help, how to name fields or data type???
data NamedParam = NamedParam
    { namedParamName  :: Name
    , namedParamParam :: SQL.Param
    }

{- | This function takes query with named parameters specified like this:

@
SELECT `name`, `user` FROM `users` WHERE `id` = :id
@

and returns either 'MySqlError' or query with all all names replaced by
questiosn marks @?@ with list of the names in the order of their appearance. For example:

>>> extractNames "SELECT * FROM `users` WHERE foo = :foo AND bar = :bar AND baz = :foo"
Right ("SELECT * FROM `users` WHERE foo = ? AND bar = ? AND baz = ?", ["foo", "bar", "foo"])
-}
extractNames
    :: SQL.Query
    -> (SQL.Query, NonEmpty Name)  -- TODO: should be either here
extractNames (SQL.Query query) = case go query of
    (_, [])         -> error "No names given"  -- TODO: later will be Either here
    (q, name:names) -> (SQL.Query q, name :| names)
  where
    go :: LByteString -> (LByteString, [Name])
    go str
        | LBS8.null str = ("", [])
        | otherwise     = case LBS8.break (== ':') str of
            (before, after) -> case LBS8.uncons after of
                Nothing -> (before, [])
                Just (':', nameStart) ->
                    let (name, remainingQuery) = LBS8.span isNameChar nameStart
                    in bimap ((before <> "?") <>) (Name (decodeUtf8 name) : ) $ go remainingQuery
                Just _ -> error "impossible happened"

    isNameChar :: Char -> Bool
    isNameChar c = isAlphaNum c || c == '_'
