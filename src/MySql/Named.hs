{-# LANGUAGE DerivingStrategies #-}

module MySql.Named
       ( NamedParam (..)
       , Name (..)
       ) where

import MySql.Error (MySqlError (..))

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
extractNames = undefined
