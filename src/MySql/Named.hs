{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module MySql.Named
       ( NamedParam (..)
       , Name (..)

       , extractNames
       , namesToRow
       ) where

import Control.Monad.Except (MonadError (throwError))
import Data.Char (isAlphaNum)
import Data.List (lookup)

import MySql.Error (MySqlError (..), WithError)
import MySql.Named.Core (Name (..), NamedParam (..))

import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Database.MySQL.Base as SQL


-- | Checks whether the 'Name' is in the list and returns
lookupName :: Name -> [NamedParam] -> Maybe SQL.Param
lookupName n = lookup n . map (\NamedParam{..} -> (namedParamName, namedParamParam))

{- | This function takes query with named parameters specified like this:

@
SELECT `name`, `user` FROM `users` WHERE `id` = :id
@

and returns either 'MySqlError' or query with all all names replaced by
questiosn marks @?@ with list of the names in the order of their appearance.

For example:

>>> extractNames "SELECT * FROM `users` WHERE foo = :foo AND bar = :bar AND baz = :foo"
("SELECT * FROM `users` WHERE foo = ? AND bar = ? AND baz = ?","foo" :| ["bar","foo"])
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
        | otherwise     = let (before, after) = LBS8.break (== ':') str in
            case LBS8.uncons after of
                Nothing -> (before, [])
                Just (':', nameStart) ->
                    let (name, remainingQuery) = LBS8.span isNameChar nameStart
                    in if LBS8.null name
                           then error "empty name"  -- TODO: Either here later
                           else bimap ((before <> "?") <>)
                                      (Name (decodeUtf8 name) :)
                                      (go remainingQuery)
                Just _ -> error "impossible happened"

    isNameChar :: Char -> Bool
    isNameChar c = isAlphaNum c || c == '_'


-- | Returns the list of values to use in query by given list of 'Name's.
namesToRow
    :: forall m . WithError m
    => NonEmpty Name  -- ^ List of the names used in query
    -> [NamedParam]   -- ^ List of the named parameters
    -> m (NonEmpty SQL.Param)
namesToRow names params = traverse magicLookup names
  where
    magicLookup :: Name -> m SQL.Param
    magicLookup n = whenNothing (lookupName n params) $ throwError $ MySqlNamedError n
