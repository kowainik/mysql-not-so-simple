{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Database.MySQL.Base (ConnectInfo (..), connectDetail)
import Database.MySQL.Connection (utf8mb4_unicode_ci)
import Hedgehog (Group, checkParallel)
import System.IO (hSetEncoding, utf8)

import MySql (executeRaw, sql)
import Test.Field (toField'fromField)
import Test.Row (toRow'fromRow)
import Test.Sql (insertSelect)


-- fucking hedgehog can't into nested groups :(
checkGroups :: [Group] -> IO ()
checkGroups groups = ifM (allM checkParallel groups) exitSuccess exitFailure

main :: IO ()
main = do
    -- Fix for the dumb terminal that circleci has
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    putTextLn "===> Establishing connection..."
    (greet, conn) <- connectDetail ConnectInfo
        { ciHost = "127.0.0.1"
        , ciPort = 3306
        , ciUser = "root"
        , ciPassword = "password"
        , ciDatabase = "test_db"
        , ciCharset = utf8mb4_unicode_ci
        }
    print greet
    putTextLn "===> Connection established..."

    varConn <- newMVar conn

-- TODO: execute schema from the file
--    schema <- readFile "sql/test.sql"
--    putStrLn schema

    print =<< executeRaw conn [sql| DROP TABLE IF EXISTS `users` |]
    print =<< executeRaw conn [sql|
        CREATE TABLE `users`
            ( `id`       INT(11) AUTO_INCREMENT PRIMARY KEY
            , `name`     TEXT      NOT NULL
            , `birthday` TIMESTAMP NOT NULL
            , `weight`   DOUBLE    NOT NULL
            , `age`      INT(11)
            ) ENGINE=InnoDB DEFAULT CHARACTER SET=utf8mb4;
    |]

    checkGroups
        [ toField'fromField
        , toRow'fromRow
        , insertSelect varConn
        ]
