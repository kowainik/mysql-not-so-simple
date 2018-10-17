import Database.MySQL.Base (ciDatabase, ciUser, connect, defaultConnectInfo)
import Hedgehog (Group, checkParallel)
import System.IO (hSetEncoding, utf8)

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
        , ciDatabase = "test_db"
        }

    checkGroups
        [ toField'fromField
        , insertSelect conn
        ]
