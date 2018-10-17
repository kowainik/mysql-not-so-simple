{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Tests for SQL queries on real DB.

module Test.Sql
       ( insertSelect
       ) where

import Data.Time.Clock (UTCTime)
import Hedgehog (Gen, Group (..), Property, forAll, property, (===))

import MySql (FromRow (..), MySQLConn, MySqlError, Only (..), Query, ToField (..), ToRow (..),
              execute_, field, query, queryRaw, sql)
import Test.Gen (genDouble, genMaybe, genText, genUtcTime, named)


insertSelect :: MySQLConn -> Group
insertSelect conn = Group "INSERT/SELECT"
    [ insertSelectProperty conn genUser  `named` "User"
    , insertSelectProperty conn genGUser `named` "GenericUser"
    ]

insertSelectProperty
    :: forall user . (FromRow user, ToRow user, Eq user, Show user)
    => MySQLConn -> Gen user -> Property
insertSelectProperty conn userG = property $ do
    -- generate random user
    user <- forAll userG

    -- insert user into DB and return id of this user
    Right [Only (userId :: Int32)] <- liftIO $ do
        execute_ conn [sql|
            INSERT INTO users (name, birthday, weight, age)
            VALUES (?, ?, ?, ?)
        |] user
        queryRawIO [sql| SELECT LAST_INSERT_ID() |]

    -- query inserted user by returned id
    Right [dbUser] <- liftIO $ queryIO [sql|
        SELECT name, birthday, weight, age
        WHERE id = ?
    |] (Only userId)

    user === dbUser
  where
    queryRawIO
        :: forall res m . (MonadIO m, FromRow res)
        => Query
        -> m (Either MySqlError [res])
    queryRawIO = runExceptT . queryRaw conn

    queryIO
        :: forall m args . (MonadIO m, ToRow args)
        => Query
        -> args
        -> m (Either MySqlError [user])
    queryIO q = runExceptT . query conn q

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
