import Hedgehog (Group, checkParallel)
import System.IO (hSetEncoding, utf8)

import Test.Field (toField'fromField)

-- fucking hedgehog can't into nested groups :(
checkGroup :: Group -> IO ()
checkGroup g = ifM (checkParallel g) exitSuccess exitFailure

main :: IO ()
main = do
    -- Fix for the dumb terminal that circleci has
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    checkGroup toField'fromField
