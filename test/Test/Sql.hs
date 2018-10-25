{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes        #-}

-- | Tests for SQL queries on real DB.

module Test.Sql
       ( insertSelect
       ) where

import Control.Concurrent.MVar (withMVar)
import Data.Time.Clock (UTCTime)
import Hedgehog (Gen, Group (..), Property, forAll, property, (===))

import MySql (FromRow (..), MySQLConn, MySqlError, NamedParam, Query, ToField (..), ToRow (..),
              asLastId, execute, field, queryNamed, sql, (=:))
import Test.Gen (genDouble, genMaybe, genText, genUtcTime, named)


insertSelect :: MVar MySQLConn -> Group
insertSelect conn = Group "INSERT/SELECT"
    [ insertSelectProperty conn genUser  `named` "User"
    , insertSelectProperty conn genGUser `named` "GenericUser"
    ]

insertSelectProperty
    :: forall user . (FromRow user, ToRow user, Eq user, Show user)
    => MVar MySQLConn -> Gen user -> Property
insertSelectProperty varConn userGenerator = property $ do
    -- generate random user
    user <- forAll userGenerator

    dbUser <- liftIO $ withMVar varConn $ \conn -> do
        -- insert user into DB
        lastId <- asLastId $ execute conn [sql|
            INSERT INTO users (name, birthday, weight, age)
            VALUES (?, ?, ?, ?)
        |] user

        -- query inserted user by returned id
        queryNamedIO conn [sql|
            SELECT name, birthday, weight, age
            FROM users
            WHERE id = :id
        |] [ "id" =: lastId ]

    Right [user] === dbUser
  where
    queryNamedIO
        :: forall m . (MonadIO m)
        => MySQLConn
        -> Query
        -> [NamedParam]
        -> m (Either MySqlError [user])
    queryNamedIO conn q = runExceptT . queryNamed conn q

data User = User
    { userName     :: Text
    , userBirthday :: UTCTime
    , userWeight   :: Double
    , userAge      :: Maybe Int32
    } deriving (Show, Eq)

instance ToRow User where
    toRow User{..} =
        [ toField userName
        , toField userBirthday
        , toField userWeight
        , toField userAge
        ]

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field

-- | Same as 'User' but derives all instances generically to test generic deriving.
data GenericUser = GenericUser
    { genericUserName     :: Text
    , genericUserBirthday :: UTCTime
    , genericUserWeight   :: Double
    , genericUserAge      :: Maybe Int32
    } deriving stock    (Show, Eq, Generic)
      deriving anyclass (ToRow, FromRow)

genAnyUser :: (Text -> UTCTime -> Double -> Maybe Int32 -> user) -> Gen user
genAnyUser constructor = constructor <$> genText <*> genUtcTime <*> genDouble <*> genMaybe

genUser :: Gen User
genUser = genAnyUser User

genGUser :: Gen GenericUser
genGUser = genAnyUser GenericUser
