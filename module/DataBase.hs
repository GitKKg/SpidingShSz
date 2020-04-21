{-# LANGUAGE TypeApplications #-}
-- | 
{-#  LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels,DuplicateRecordFields #-}

module DataBase (
  -- Stock (Stock) will just export constructor, no field fucntion like _code,_date
    Stock (..), -- export everything in Stock definition
    BonusInfo (..),
    AllotmentInfo (..),
    RightInfo,
    defaultStock,
    defaultBonusInfo,
    defaultAllotmentInfo,
    testPg
) where

import Database.Selda   --proxychains stack install selda-0.4.0.0
import Database.Selda.SQLite
import Control.Exception
import Data.Typeable

import Data.Text

-- sudo apt-get install libpq-dev, proxychains stack install selda-postgresql
import Database.Selda.PostgreSQL
import Database.Selda.Backend.Internal
import Debug.Trace

data Stock = Stock  -- the field member name must be exact same with field of table in database which already exist
-- order no matter, just parts no matter, only name and type matter
  {
    -- name :: Text,
    _code :: Text,
    _date :: Int,
    _name :: Text,
    _shares :: Int,-- use Int not Double,for saving space of database
    _value :: Int,
    _factor :: Int, -- all use Int, * 1000 to keep 3 decimal places,no way
    _open :: Int,
    _high :: Int,
    _close :: Int,
    _low :: Int,
    _average :: Int,
    -- can't use Int, because old days data will be very small,3 decimal places are not enough
    _fuquan_average :: Double 
  } deriving (Generic) -- not deriving Show, for default show not show utf8 Chinese correcttly

instance SqlRow Stock

instance Show Stock where
  show stock = "Stock" ++ " {"++
    "\n_code =" ++ (unpack . (_code :: Stock -> Text) $ stock) ++
    ",\n_date =" ++ (show . _date $ stock) ++
    -- must use unpack, using show can't show Chinese 
    ",\n_name =" ++ (unpack . (_name :: Stock -> Text) $ stock) ++ 
    ",\n_open =" ++ (show . (/ 1000) . (fromIntegral :: Int -> Float). _open $ stock) ++
    ",\n_high =" ++ (show . (/ 1000) . (fromIntegral :: Int -> Float). _high $ stock) ++
    ",\n_close =" ++ (show . (/ 1000) . (fromIntegral :: Int -> Float). _close $ stock) ++
    ",\n_low =" ++ (show . (/ 1000) . (fromIntegral :: Int -> Float). _low $ stock)  ++
    ",\n_shares =" ++ (show  . _shares $ stock) ++
    ",\n_value =" ++ (show  . _value $ stock) ++
    ",\n_average =" ++ (show  . _average $ stock) ++
    ",\n_fuquan_average =" ++ (show  . _fuquan_average $ stock) ++
    "\n}\n"
defaultStock = Stock "600000" 20200101 "浦发银行" 0 0 0 0 0 0 0 0 0

data BonusInfo = BonusInfo -- dividend and sharesent, 分红 送股 表
  {
    _codeB :: Text,
    _nameB :: Text,
    _announceDateB :: Int, -- B means Bonus 公告日期
    _bonusSharesRatio :: Int,--Double , -- ratio of shares sent per 10 shares, 送股比例 每10股送股数
    -- use Int not Double,for saving space of database,keep 3 decimal places
    _sharesTranscent :: Int,--Double, -- ratio of shares transcent per 10 shares ,transfer capital common reserve to share capital  转增股比例 每10股转增股数 
    _dividend :: Int, -- RMB Yuan sent per 10 shares, 每10股分红
    _process :: Bool, -- True means already did,False means just in plan
    _exRightDateB :: Int, -- 除权日
    _recordDateB :: Int  -- 股权登记日
  } deriving (Generic,Show)

instance SqlRow BonusInfo


defaultBonusInfo = BonusInfo "600000"  "工商银行" 20200101 0 0 0 False 0 0

data AllotmentInfo = AllotmentInfo --  Allotment,  配股 表
  {
    _codeA :: Text,
    _nameA :: Text,
    _announceDateA :: Int, -- A means Allotment
    _allotRatio :: Int, -- Ratio , 配股比例 每10股可买股数
    _offerPrice :: Int, -- 配股价格
    _capStock :: Int, -- 基准股本
    _exRightDateA :: Int, -- 除权日
    _recordDateA :: Int -- 股权登记日
  } deriving (Generic,Show)

instance SqlRow AllotmentInfo
defaultAllotmentInfo = AllotmentInfo "600000"  "工商银行" 20200101 0  0 0 0 0
data UpdateTime = UpdateTime
  {
    _time :: Int
  } deriving (Generic)

instance SqlRow UpdateTime

type RightInfo = Either BonusInfo AllotmentInfo

data Pet = Dog | Horse | Dragon
  deriving (Show, Read, Bounded, Enum)
instance SqlType Pet

data Person = Person
  {
   -- pid  :: ID Person
  name :: Text
  , age :: Int
  , pet :: Maybe Pet
  } deriving (Generic,Show)
instance SqlRow Person

people :: Table Person
people = table "people" [#name :+ #pet :- unique]

insertSara :: SeldaM  () ()
insertSara = insert_ people [Person  "Sara" 14 Nothing]

insertThenInspect :: SeldaM () [Person]
insertThenInspect = do
  insertSara
  query (select people)

-- Create databse
-- initdb.exe -D E:\Tool\PgSqlDB -E UTF-8 -U Kant -W

-- start running database
-- pg_ctl -D  E:\Tool\PgSqlDB\db  -l db.log start

-- edit pg_hba.conf of server to permmit remote connection by user Kant
-- # IPv4 local connections:
-- host    all             Kant             0.0.0.0/0            password
-- edit postgresql.conf of server to permmit listening all IP
--listen_addresses = '*'

pgConnectInfo = "postgres" `on` "192.168.51.212" `auth` ("Kant", "123456")
testPg = (withPostgreSQL :: PGConnectInfo -> SeldaT PG IO () -> IO ()) pgConnectInfo $ do
  tryDropTable people
  tryCreateTable people
  enum <- liftIO . (try :: IO (SeldaT PG IO Int) -> IO (Either SeldaError (SeldaT PG IO Int))) . return $ insert people -- can't catch exception
    [Person  "Kant"      10 (Just Horse),
      Person  "Velvet"    19 (Just Dog)
    , Person  "Kobayashi" 23 (Just Dragon)
    , Person  "Miyu"      10 Nothing
    
    ]
  num <- case enum of
    Left e ->  return 0
    Right n -> n
  traceM $ "1st time ,inserted " <> show num <> " rows"

  num <- tryInsert people [Person  "Kant"      10 (Just Horse)]
  liftIO $ print $ "2nd time inserted " <> show num <> " rows"
  traceM $ "2nd time,inserted " <> show num <> " rows"
  traceM $ "wait for input"
  liftIO $ do
    input <- getLine
    putStrLn $ "you inputed " ++ input
  adultsAndTheirPets <- transaction $ query $ do
    person <- select people
    restrict (person ! #age .>= 18)
    return (person ! #name)-- :*: person ! #pet :*: person ! #age)
    -- return person
  liftIO $ print adultsAndTheirPets
  traceM $ show adultsAndTheirPets
  peopleWithHorse <- query $ do
    person <- select people
    restrict (person ! #pet .==  literal (Just Horse)) -- see Expressions over columns of Database.Selda document
    return (person ! #name)-- :*: person ! #pet :*: person ! #age)
  traceM $ show peopleWithHorse
  --dropTable people --
--  testPg
-- "inserted True rows"
-- ["Velvet","Kobayashi"]

--  testPg
-- "inserted False rows"
-- ["Velvet","Kobayashi"]

-- sudo proxychains  snap install dbeaver-ce  , database tool ,better than pgadimin

testPg2 :: IO ()
testPg2 = do
  pgCon <- pgOpen pgConnectInfo :: IO (SeldaConnection PG)
  --runSeldaT (tryDropTable people) pgCon
  enum <- (try :: IO Int -> IO (Either SeldaError  Int )) $
    runSeldaT (do
                  tryCreateTable people
                  insert people [Person  "Kant" 10 (Just Dragon)])
    pgCon
  num <- case enum of
    Left e ->  do
      traceM $ "exception :" ++ show e ++ ", just return 0"
      return 0
    Right n -> return n
  traceM $ "1st time ,inserted " <> show num <> " rows"
  num <- runSeldaT (do
                deleteFrom people (\person -> person ! #pet .== just (literal Dog) )
                -- if you can't give a then b seperatly, you can consider giving a -> b as a function!
                )
    pgCon
  traceM $ "deleted " ++ show num ++ "row\n"
  seldaClose pgCon
  return ()
