{-# LANGUAGE DerivingStrategies #-}

{- | Core data types used for Named parameters -}

module MySql.Named.Core
       ( Name (..)
       , NamedParam (..)
       ) where

import qualified Database.MySQL.Base as SQL


-- | Wrapper over name of the argument.
newtype Name = Name
    { unName :: Text
    } deriving newtype (Show, Eq, Ord)

-- please, send help, how to name fields or data type???
data NamedParam = NamedParam
    { namedParamName  :: Name
    , namedParamParam :: SQL.Param
    }
