{-# LANGUAGE DerivingStrategies #-}

{- | Core data types used for Named parameters -}

module MySql.Named.Core
       ( Name (..)
       ) where

-- | Wrapper over name of the argument.
newtype Name = Name
    { unName :: Text
    } deriving newtype (Show, Eq, Ord)
