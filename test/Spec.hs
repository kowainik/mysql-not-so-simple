{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Database.MySQL.Base (ciDatabase, ciPassword, ciUser, connect, defaultConnectInfo)
import Hedgehog (Group, checkParallel)
import System.IO (hSetEncoding, utf8)

import MySql (MySqlError, Only, executeRaw, queryRaw, sql)
import Test.Field (toField'fromField)
import Test.Sql (insertSelect)

import qualified Database.MySQL.Base as SQL
import qualified System.IO.Streams as Streams

-- fucking hedgehog can't into nested groups :(
checkGroups :: [Group] -> IO ()
checkGroups groups = ifM (allM checkParallel groups) exitSuccess exitFailure

main :: IO ()
main = do
    -- Fix for the dumb terminal that circleci has
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    putTextLn "===> Establishing connection..."
    conn <- connect defaultConnectInfo
        { ciUser = "root"
        , ciPassword = "password"
        , ciDatabase = "test_db"
        }
    varConn <- newMVar conn
    putTextLn "===> Connection established..."

    schema <- readFile "sql/test.sql"
    print =<< executeRaw conn (fromString schema)
    putTextLn "===> Test schema created..."

    print =<< SQL.executeMany conn
      "INSERT INTO `users` (`name`, `weight`, `age`) VALUES (?,?,?)"
      [ [ SQL.MySQLText "Dima"
        , SQL.MySQLDouble 80.3
        , SQL.MySQLInt32 100
        ]
      , [ SQL.MySQLText "Ivan"
        , SQL.MySQLDouble 60
        , SQL.MySQLInt32 20
        ]

      ]
--    print =<< executeRaw conn [sql|
--        INSERT INTO `users` (`name`, `birthday`, `weight`, `age`)
--        VALUES ('Dima', '2018-10-18 15:49', 75.9, 100)
--    |]
    putTextLn "===> Test data inserted..."

--    res :: Either MySqlError [Only Int32] <-
--        runExceptT $ queryRaw conn [sql| SELECT LAST_INSERT_ID(); |]
--    (defs, is) <- SQL.query_ conn "SELECT LAST_INSERT_ID();"
    (defs, is) <- SQL.query_ conn "SELECT * FROM users"
    putTextLn "===> ID queried..."
    print =<< Streams.toList is

    (defs, is) <- SQL.query_ conn "SELECT * FROM users"
    putTextLn "===> ID queried..."
    print =<< Streams.toList is

    putTextLn "Tests finished"

--     checkGroups
--         [ toField'fromField
--         , insertSelect varConn
--         ]
