{-# LANGUAGE FlexibleContexts #-}

module MySql.Query
       ( -- * Queries
         execute
       , execute_
       , executeNamed
       , executeNamed_
       , executeRaw
       , executeRaw_
       , executeMany
       , executeMany_
       , executeManyRaw
       , executeManyRaw_
       , executeFile
       , executeFiles
       , query
       , queryRaw
       , queryNamed

       -- * Utils
       , asLastId

       -- * Reexports from @mysql-haskell library
       , SQL.OK (..)
       , MySQLConn
       , MySQLValue (..)
       , Query
       , Param
       , ConnectInfo (..)
       , close
       , connect
       , defaultConnectInfoMB4
       ) where

import Control.Monad.Except (MonadError (throwError), liftEither)
import Data.Sequence (Seq (..), (|>))
import Database.MySQL.Base (ConnectInfo (..), MySQLConn, MySQLValue (..), OK, Param, Query, close,
                            connect, defaultConnectInfoMB4)

import MySql.Error (WithError)
import MySql.Field (LastId (..))
import MySql.Matcher (mkMatcherState, usingMatcher)
import MySql.Named (NamedParam, extractNames, namesToRow)
import MySql.Row (FromRow (..), ToRow (..))

import qualified Database.MySQL.Base as SQL
import qualified System.IO.Streams as Stream

-- | Execute given query and returning meta information about the result.
execute :: (MonadIO m, ToRow row) => MySQLConn -> Query -> row -> m OK
execute conn q = liftIO . SQL.execute conn q . toRow

-- | Like 'execute' but ignores the result.
execute_ :: (MonadIO m, ToRow row) => MySQLConn -> Query -> row -> m ()
execute_ conn q = void . execute conn q

-- | Execute a MySQL query which return a result-set with parameters.
executeNamed :: (MonadIO m, WithError m) => MySQLConn -> Query -> [NamedParam] -> m OK
executeNamed conn qNamed namedArgs =
    withNamedArgs qNamed namedArgs >>= uncurry (execute conn)

-- | Like 'executeNamed' but ignores the result.
executeNamed_ :: (MonadIO m, WithError m) => MySQLConn -> Query -> [NamedParam] -> m ()
executeNamed_ conn q = void . executeNamed conn q

-- | Execute a MySQL query which don't return a result-set.
executeRaw :: MonadIO m => MySQLConn -> Query -> m OK
executeRaw conn = liftIO . SQL.execute_ conn

-- | Like 'executeRaw' but ignores the result.
executeRaw_ :: MonadIO m => MySQLConn -> Query -> m ()
executeRaw_ conn = void . executeRaw conn

-- | Execute all commands from the file.
executeFile :: (MonadIO m) => MySQLConn -> FilePath -> m ()
executeFile conn file = executeFiles conn [file]

-- | Execute all commands from the list of the given files.
executeFiles :: MonadIO m => MySQLConn -> [FilePath] -> m ()
executeFiles conn files = do
    filesQ <- mconcat <$> forM files readFile
    executeManyRaw_ conn $ fromString filesQ

-- | Execute a multi-row query which don't return result-set.
executeMany :: (MonadIO m, ToRow row) => MySQLConn -> Query -> [row] -> m [OK]
executeMany conn q = liftIO . SQL.executeMany conn q . map toRow

-- | Like 'executeMany' but ignores the result.
executeMany_ :: (MonadIO m, ToRow row) => MySQLConn -> Query -> [row] -> m ()
executeMany_ conn q = void . executeMany conn q

-- | Like 'executeMany' but doesn't take row arguments.
executeManyRaw :: (MonadIO m) => MySQLConn -> Query -> m [OK]
executeManyRaw conn = liftIO . SQL.executeMany_ conn

-- | Like 'executeManyRaw' but ignores the result.
executeManyRaw_ :: MonadIO m => MySQLConn -> Query -> m ()
executeManyRaw_ conn = void . executeManyRaw conn

-- | Execute a MySQL query which return a result-set with parameters.
query
    :: (MonadIO m, WithError m, ToRow args, FromRow res)
    => MySQLConn -> Query -> args -> m [res]
query conn q args = liftIO (SQL.query conn q $ toRow args) >>= fromRows

-- | Execute a MySQL query which return a result-set.
queryRaw :: (MonadIO m, WithError m, FromRow res) => MySQLConn -> Query -> m [res]
queryRaw conn q = liftIO (SQL.query_ conn q) >>= fromRows

queryNamed
    :: (MonadIO m, WithError m, FromRow res)
    => MySQLConn -> Query -> [NamedParam] -> m [res]
queryNamed conn qNamed namedArgs =
    withNamedArgs qNamed namedArgs >>= uncurry (query conn)

-- | Useful function to extract last inserted id from OK result.
asLastId :: Functor f => f OK -> f LastId
asLastId = fmap (LastId . SQL.okLastInsertID)

----------------------------------------------------------------------------
-- Low-level internal details
----------------------------------------------------------------------------

{- | Helper function to help parse the rows and read the `InputStream` till its
end or fail early if the parsing fails.
-}
fromRows
    :: forall a m . (MonadIO m, FromRow a, WithError m)
    => ([SQL.ColumnDef], Stream.InputStream [SQL.MySQLValue])
    -> m [a]
fromRows (_columnDefs, iStream) = toList <$> go Empty
  where
    go :: Seq a -> m (Seq a)
    go acc = liftIO (Stream.read iStream) >>= \case
        -- There are no more rows to be read from the server
        Nothing -> pure acc
        -- There are still rows to be read from the server
        Just values -> case usingMatcher (mkMatcherState values) fromRow of
            -- Parsing of current row succeeded. Recurse
            Right parsedValue -> go (acc |> parsedValue)
            -- Parsing of the current row failed, error out
            Left err -> do
                -- You need to 'consume' the while inputstream if we want to discard results midway
                -- to prevent errors
                liftIO $ SQL.skipToEof iStream
                throwError err

withNamedArgs :: WithError m => Query -> [NamedParam] -> m (Query, NonEmpty Param)
withNamedArgs qNamed namedArgs = do
    (q, names) <- liftEither $ extractNames qNamed
    args <- namesToRow names namedArgs
    pure (q, args)
