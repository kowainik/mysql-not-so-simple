{- | Implements monad for writing 'FromRow' instances in a more convenient way.
-}

module MySql.Matcher
       ( Matcher (..)
       , Row
       , usingMatcher
       , field
       ) where

import Control.Monad.Except (MonadError (throwError))

import MySql.Error (MySqlError (..))
import MySql.Field (FromField (..))

import qualified Database.MySQL.Base as SQL


type Row = [SQL.MySQLValue]

{- | Monad for parsing tables rows into custom data types.

@
State Row (Either MySqlError) a
    = Row -> Either MySqlError (a, Row)
@
-}
newtype Matcher a = Matcher
    { runMatcher :: StateT Row (Either MySqlError) a
    } deriving (Functor, Applicative, Monad, MonadState Row, MonadError MySqlError)

-- | Runs matcher over given row and returns either error or result.
usingMatcher :: Row -> Matcher a -> Either MySqlError a
usingMatcher row matcher = do  -- using do-notation for Either monad
    (a, remainFields) <- usingStateT row $ runMatcher matcher
    case remainFields of
        []   -> Right a
        x:xs -> Left $ MySqlExpectedEndOfRow (x :| xs)

-- | Parser for single field.
field :: FromField a => Matcher a
field = get >>= \case
    [] -> throwError MySqlUnexpectedEndOfRow
    val:vals -> case fromField val of
        Nothing -> throwError $ MySqlWrongField val
        Just x  -> put vals >> pure x
