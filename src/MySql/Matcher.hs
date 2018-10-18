{-# LANGUAGE RecordWildCards #-}

{- | Implements monad for writing 'FromRow' instances in a more convenient way.
-}

module MySql.Matcher
       ( Matcher (..)
       , MatcherState (..)
       , Row

       , mkMatcherState
       , usingMatcher
       , field
       ) where

import Control.Monad.Except (MonadError (throwError))

import MySql.Error (MySqlError (..))
import MySql.Field (FromField (..))

import qualified Database.MySQL.Base as SQL


type Row = [SQL.MySQLValue]

data MatcherState = MatcherState
    { matcherStatePos :: Int
    , matcherStateRow :: Row
    }

mkMatcherState :: Row -> MatcherState
mkMatcherState = MatcherState 0

{- | Monad for parsing tables rows into custom data types.

@
State Row (Either MySqlError) a
    = Row -> Either MySqlError (a, Row)
@
-}
newtype Matcher a = Matcher
    { runMatcher :: StateT MatcherState (Either MySqlError) a
    } deriving (Functor, Applicative, Monad, MonadState MatcherState, MonadError MySqlError)

-- | Runs matcher over given row and returns either error or result.
usingMatcher :: MatcherState -> Matcher a -> Either MySqlError a
usingMatcher matcherState matcher = do  -- using do-notation for Either monad
    (a, finalState) <- usingStateT matcherState $ runMatcher matcher
    case matcherStateRow finalState of
        []   -> Right a
        x:xs -> Left $ MySqlExpectedEndOfRow (x :| xs)

  -- | Parser for single field.
field :: FromField a => Matcher a
field = do
    MatcherState{..} <- get
    case matcherStateRow of
        [] -> throwError MySqlUnexpectedEndOfRow
        val:vals -> case fromField val of
            Left e  -> throwError $ MySqlWrongColumn matcherStatePos e
            Right x -> x <$ put (MatcherState (matcherStatePos + 1) vals)
