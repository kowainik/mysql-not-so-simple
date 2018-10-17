module Main (main) where

import Database.MySQL.Base (ciDatabase, ciPassword, ciUser, connect, defaultConnectInfo)
import Hedgehog (Group, checkParallel)
import System.IO (hSetEncoding, utf8)

import MySql (executeRaw_)
import Test.Field (toField'fromField)
import Test.Sql (insertSelect)

-- fucking hedgehog can't into nested groups :(
checkGroups :: [Group] -> IO ()
checkGroups groups = ifM (allM checkParallel groups) exitSuccess exitFailure

main :: IO ()
main = do
    -- Fix for the dumb terminal that circleci has
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    conn <- connect defaultConnectInfo
        { ciUser = "root"
        , ciPassword = "password"
        , ciDatabase = "test_db"
        }
    varConn <- newMVar conn

    schema <- readFile "sql/test.sql"
    executeRaw_ conn (fromString schema)

    checkGroups
        [ toField'fromField
        , insertSelect varConn
        ]
