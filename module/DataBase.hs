{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
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
    getStockCodes,
    saveStockPrice,
    saveBonusInfo,
    saveAllotmentInfo
) where
import Data.Maybe
import Data.String
import Database.Selda   --proxychains stack install selda-0.4.0.0
import Database.Selda.SQLite
import Control.Exception
import Data.Typeable

import Data.Text

-- sudo apt-get install libpq-dev, proxychains stack install selda-postgresql
import Database.Selda.PostgreSQL
import Database.Selda.Backend.Internal
import Debug.Trace
import Control.Monad
import Data.Data

import System.Directory
import Text.Regex.TDFA
import qualified Data.List  as DL

import DebugLogM

import qualified Data.Set as DS 

import Control.Monad.State
import Control.Monad.State.Lazy

import Control.Concurrent
import DebugLogM

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

stockPriceT :: Table Stock
stockPriceT = table "stock" [#_code :+ #_date :- unique]

bonusInfoT :: Table BonusInfo
bonusInfoT = table "bonus" [#_codeB :+ #_announceDateB  :- unique]
-- ocaasionly ,we get no exRightDate when it is latest,so could not join it in unique
allotmentT :: Table AllotmentInfo
allotmentT = table "allot" [#_codeA :+ #_announceDateA  :- unique]

people :: Table Person
people = table "people" [#name :+ #pet :- unique]

insertSara :: SeldaM  () ()
insertSara = insert_ people [Person  "Sara" 14 Nothing]

insertThenInspect :: SeldaM () [Person]
insertThenInspect = do
  insertSara
  query (select people)

-- Create databse
-- initdb.exe -D E:\Tool\PgSqlDB\db -E UTF-8 -U Kant -W

-- start running database
-- pg_ctl -D  E:\Tool\PgSqlDB\db  -l db.log start
-- pg_ctl -D  H:\PgSqlDB\db  -l H:\PgSqlDB\log\db.log start

-- edit pg_hba.conf of server to permmit remote connection by user Kant
-- # IPv4 local connections:
-- host    all             Kant             0.0.0.0/0            password
-- edit postgresql.conf of server to permmit listening all IP
--listen_addresses = '*'

-- sudo chmod 777 /var/run/postgresql
-- initdb is here :  /usr/lib/postgresql/12/bin/
-- emacs pg_hba.conf
-- # IPv4 local connections:
-- host    all             Kant             127.0.0.1/32            password
-- /usr/lib/postgresql/12/bin$ ./pg_ctl -D /media/sdb1/PgSqlDB/stockDB -l /media/sdb1/PgSqlDB/log/stockDb.log start  , stop
-- if start failied with permmsion denied:
-- sudo chown -R kyle /var/run/postgresql , kyle here is your login user name
-- sudo systemctl stop postgresql@11-main.service
-- sudo systemctl disable postgresql@11-main.service
-- sudo systemctl stop postgresql@12-main.service
-- sudo systemctl disable postgresql@12-main.service

-- sudo su - postgres -c "createuser Kant --createdb"
-- sudo emacs /etc/postgresql/12/main/pg_hba.conf
-- Local all postgres peer to Local all postgres trust

-- pgConnectInfo = "postgres" `on` "192.168.51.212" `auth` ("Kant", "123456")
-- pgConnectInfo = "postgres" `on` "192.168.1.2" `auth` ("Kant", "123456")
pgConnectInfo = "postgres" `on` "127.0.0.1" `auth` ("Kant", "Kant")
-- (withPostgreSQL :: PGConnectInfo -> SeldaT PG IO () -> IO ())
testPg = withPostgreSQL @IO  pgConnectInfo $ do
  tryDropTable people
  tryCreateTable people
  -- (try :: IO (SeldaT PG IO Int) -> IO (Either SeldaError (SeldaT PG IO Int)))
  -- see @ make you type so much less words
  enum <- liftIO . try @SeldaError . return $ insert people -- can't catch exception
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
  -- pgOpen pgConnectInfo :: IO (SeldaConnection PG)
  pgCon <- pgOpen pgConnectInfo
  --runSeldaT (tryDropTable people) pgCon
  --(try :: IO Int -> IO (Either SeldaError  Int ))
  -- see @ make you how much less type!
  enum <- try @SeldaError $
    runSeldaT (do
                  tryCreateTable people
                  insert people [Person  "Kant" 10 (Just Dragon)])
    pgCon
  num <- case enum of
    Left e ->  do
      traceM $ "exception : \n" ++ show e ++ ",\njust return 0"
      return 0
    Right n -> return n
  traceM $ "1st time ,inserted " <> show num <> " rows"
  num <- runSeldaT (do
                deleteFrom people (\person -> person ! #pet .== just (literal Dog) )
                -- if you can't give a then b sepaeratly, you can consider giving a -> b as a function!
                )
    pgCon
  traceM $ "deleted " ++ show num ++ "row\n"
  seldaClose pgCon
  return ()

-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- introduce toConstrt to compare value constructor of SeldaError,so we can judge specific exception status,such DbError or SqlError
deriving instance Data SeldaError

--tempSPt :: Table Stock
--tempSPt = table "tempSPt" [#_code :+ #_date :- unique]

-- test demo
-- s1 = defaultStock {_date=20180906}
-- s2 = defaultStock {_date=20180908}
-- saveStockPrice [s1]
-- saveStockPrice [s1,s2]

saveStockPrice :: String -> [Stock] -> IO()
saveStockPrice threadId stockData = do
  logOutM $ "saveStockPrice,Stock code is " ++ (unpack .(_code :: Stock -> Text) .DL.head) stockData ++ "\n"
  pgCon <- pgOpen pgConnectInfo
  -- num <- runSeldaT (do
  --                      tryCreateTable stockPriceT
  --                      upsert stockPriceT (\_ -> literal True) (\row -> row ) stockData) pgCon
  --traceM $ "upsert " ++ show num ++ " rows"

  -- queryInto :: (MonadSelda m, Relational a) => Table a -> Query (Backend m) (Row (Backend m) a) -> m Int
  -- different thread use different id,or else make conflict when drop or insert table with same name at the same time
  --let tempSPt  = table (fromString $ "temSpt" ++ threadId) [#_code :+ #_date :- unique]

  -- distinct with table receate method seem not work, and cost really much on Hard disk usage
  -- have to use try insert one by one way
  num <- runSeldaT (do
                       tryCreateTable stockPriceT
                       boolL <- traverse ((tryInsert stockPriceT) . (: [])) stockData
                       return . DL.length . (DL.filter (\x -> x == True) ) $  boolL

                   )
                                              
                   
         pgCon
  logOutM $ "inserted " ++ show num ++ " rows into stock table\n"
  
  --traceM $ "1st time ,inserted " <> show num <> " rows"
  --num <- runSeldaT (upsert stockPriceT (\_ -> literal True) (\row -> row ) stockData) pgCon
  --traceM $ "upsert " ++ show num ++ " rows"
  seldaClose pgCon
  --traceM $ "inserted " <> show num <> " rows"


-- test demo
-- s1 = defaultBonusInfo {_announceDateB=20180906}
-- s2 = defaultBonusInfo {_announceDateB=20180909}
-- saveBonusInfo [s1]
-- saveBonusInfo [s1,s2]

--tempBt :: Table BonusInfo
--tempBt  = table "tempBonusT" [#_codeB :+ #_announceDateB  :- unique]

saveBonusInfo :: String -> [BonusInfo] -> IO()
saveBonusInfo threadId bT = do
  pgCon <- pgOpen pgConnectInfo
  -- num <- runSeldaT (do
  --                      tryCreateTable stockPriceT
  --                      upsert stockPriceT (\_ -> literal True) (\row -> row ) stockData) pgCon
  --traceM $ "upsert " ++ show num ++ " rows"

  -- queryInto :: (MonadSelda m, Relational a) => Table a -> Query (Backend m) (Row (Backend m) a) -> m Int
  -- different thread use different id,or else make conflict when drop or insert table with same name at the same time
  num <- runSeldaT (do
                       tryCreateTable bonusInfoT
                       boolL <- traverse ((tryInsert bonusInfoT) . (: [])) bT
                       return . DL.length . (DL.filter (\x -> x == True) ) $  boolL

                   )
                                              
                   
         pgCon
  logOutM $ "inserted " ++ show num ++ " rows into bonus table\n"
  seldaClose pgCon
  --traceM $ "inserted " <> show num <> " rows"

-- test demo
-- s1 = defaultAllotmentInfo {_announceDateA=20180906}
-- s2 = defaultAllotmentInfo {_announceDateA=20180909}
-- saveAllotmentInfo [s1]
-- saveAllotmentInfo [s1,s2]

--tempAt :: Table AllotmentInfo
--tempAt  = table "tempAllotmentT" [#_codeA :+ #_announceDateA  :- unique]

saveAllotmentInfo :: String -> [AllotmentInfo] -> IO()
saveAllotmentInfo threadId aT = do
  pgCon <- pgOpen pgConnectInfo
  -- num <- runSeldaT (do
  --                      tryCreateTable stockPriceT
  --                      upsert stockPriceT (\_ -> literal True) (\row -> row ) stockData) pgCon
  --traceM $ "upsert " ++ show num ++ " rows"

  -- queryInto :: (MonadSelda m, Relational a) => Table a -> Query (Backend m) (Row (Backend m) a) -> m Int
  -- different thread use different id,or else make conflict when drop or insert table with same name at the same time
  num <- runSeldaT (do
                       tryCreateTable allotmentT
                       boolL <- traverse ((tryInsert allotmentT) . (: [])) aT
                       return . DL.length . (DL.filter (\x -> x == True) ) $  boolL

                   )
                                              
                   
         pgCon
  logOutM $ "inserted " ++ show num ++ " rows into  allotment table\n"
  seldaClose pgCon
  --traceM $ "inserted " <> show num <> " rows"

-- form TDX day files, maybe should get codes from official site of stock exchange in future
-- getStockCodes "./module/sinaCodes"
getStockCodes :: FilePath -> IO [String]
getStockCodes fp = do
  flist <-listDirectory fp
  -- use DS to make deduplication,just like python ever did
  -- use not null to remove non-digital file name
  traverse return $ DS.toList . DS.fromList. DL.sort . DL.filter (not .DL.null) .  DL.filter rmShitStock . fmap (matchNumber. matchCode) $ flist
  where
    --  just fucking weird , :: could not pass, must use @ typeApplication
    -- add ^sh and ^sz to remove 0003000 such shit code,it's index of Shanghai ShenZhen, does not exist in 163 stock,trigger exception when parse
    matchCode file =  (=~) @FilePath @String @String  file  "^sh6[0-9]+|^sz0[0-9]+"
    matchNumber preCode = (=~) @String @String @String preCode "[0-9]+"
    rmShitStock code = not $ (=~) @String @String @Bool code "^2|^3|^9|^010^019|^1|^4|^5|^7|^8"
    -- another way, use negative match
    --rmShitStock code =  (=~) @String @String @Bool code "^[^(2|3|9)]"

getLatestDate :: String ->  IO (Maybe Int)
getLatestDate code = do
  pgCon <- pgOpen pgConnectInfo
  dateInNum <- runSeldaT (do
                       tryCreateTable stockPriceT
                       date <- query $ do
                         stockL <- select stockPriceT
                         restrict (stockL ! #_code .== literal (pack code))
                         order (stockL ! #_date) descending
                         return $ stockL ! #_date
                       if DL.length date /= 0
                         then return . Just $ date !! 0
                         else return Nothing
                       ) pgCon
  seldaClose pgCon
  return dateInNum


-- should tranverse List of announceDate and recordDate from the latest to earliest ,to update factors of prices zone by zone,and zone is divided by these date groups
getAnDateLbt :: String ->  IO (Maybe [Int])
getAnDateLbt code = do
  pgCon <- pgOpen pgConnectInfo
  dateInNum <- runSeldaT (do
                       tryCreateTable bonusInfoT
                       date <- query $ do
                         stockL <- select bonusInfoT
                         restrict (stockL ! #_codeB .== literal (pack code))
                         order (stockL ! #_announceDateB) descending
                         return $ stockL ! #_announceDateB
                       if DL.length date /= 0
                         then return . Just $ date -- !! 0
                         else return Nothing
                       ) pgCon

  seldaClose pgCon
  return dateInNum

getAnDateLat :: String ->  IO (Maybe [Int])
getAnDateLat code = do
  pgCon <- pgOpen pgConnectInfo
  dateInNum <- runSeldaT (do
                       tryCreateTable allotmentT
                       date <- query $ do
                         stockL <- select allotmentT
                         restrict (stockL ! #_codeA .== literal (pack code))
                         order (stockL ! #_announceDateA) descending
                         return $ stockL ! #_announceDateA
                       if DL.length date /= 0
                         then return . Just $ date  -- !! 0
                         else return Nothing
                       ) pgCon

  seldaClose pgCon
  return dateInNum

getReDateLbt :: String ->  IO (Maybe [Int])
getReDateLbt code = do
  pgCon <- pgOpen pgConnectInfo
  dateInNum <- runSeldaT (do
                       tryCreateTable bonusInfoT
                       date <- query $ do
                         stockL <- select bonusInfoT
                         restrict (stockL ! #_codeB .== literal (pack code))
                         order (stockL ! #_recordDateB) ascending
                         return $ stockL ! #_recordDateB
                       if DL.length date /= 0
                         then return . Just $ date -- !! 0
                         else return Nothing
                       ) pgCon
  seldaClose pgCon
  return dateInNum

getReDateLat :: String ->  IO (Maybe [Int])
getReDateLat code = do
  pgCon <- pgOpen pgConnectInfo
  dateInNum <- runSeldaT (do
                       tryCreateTable allotmentT
                       date <- query $ do
                         stockL <- select allotmentT
                         restrict (stockL ! #_codeA .== literal (pack code))
                         order (stockL ! #_recordDateA) ascending
                         return $ stockL ! #_recordDateA
                       if DL.length date /= 0
                         then return . Just $ date -- !! 0
                         else return Nothing
                       ) pgCon
  seldaClose pgCon
  return dateInNum

getExDateLat :: String ->  IO (Maybe [Int])
getExDateLat code = do
  pgCon <- pgOpen pgConnectInfo
  dateInNum <- runSeldaT (do
                       tryCreateTable allotmentT
                       date <- query $ do
                         stockL <- select allotmentT
                         restrict (stockL ! #_codeA .== literal (pack code))
                         order (stockL ! #_exRightDateA) ascending
                         return $ stockL ! #_exRightDateA
                       if DL.length date /= 0
                         then return . Just $ date -- !! 0
                         else return Nothing
                       ) pgCon
  seldaClose pgCon
  return dateInNum

getExDateLbt :: String ->  IO (Maybe [Int])
getExDateLbt code = do
  pgCon <- pgOpen pgConnectInfo
  dateInNum <- runSeldaT (do
                       tryCreateTable bonusInfoT
                       date <- query $ do
                         stockL <- select bonusInfoT
                         restrict (stockL ! #_codeB .== literal (pack code))
                         order (stockL ! #_exRightDateB) ascending
                         return $ stockL ! #_exRightDateB
                       if DL.length date /= 0
                         then return . Just $ date -- !! 0
                         else return Nothing
                       ) pgCon
  seldaClose pgCon
  return dateInNum


getPrDateL :: String -> IO [Int]
getPrDateL code = do
  withPostgreSQL @IO  pgConnectInfo $ do
    tryCreateTable stockPriceT
    query $ do
      stockL <- select stockPriceT
      restrict (stockL ! #_code .== literal (pack code))
      order (stockL ! #_date) ascending
      return $ stockL ! #_date


-- for ordered lists ,merge sorts
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) = if x < y
                        then x:(merge xs (y:ys))
                        else y:(merge (x:xs) ys)
merge [] xs = xs
merge xs [] = xs


updateAverage :: IO (Int)
updateAverage = do
  withPostgreSQL @IO  pgConnectInfo $ do
    tryCreateTable stockPriceT
    --codeL <- query $ do
    --  _code `from` select stockPriceT
    update stockPriceT (const $ literal True)
      (\r -> r `with` [ #_average :=   (r ! #_value) * 100000 /  (r ! #_shares)  ] )
      -- value in in 10 thousnad RMB Yuan
      -- shares in 100, one hand stock, so 10000/100 = 100
      -- mul to more 1000 to get 3 decimal digital space
      -- Col s a get instance of Num and Fractional, so * 100000  and / are available
    
data ABRcDate = AllotRcDate | BonusRcDate deriving (Eq,Show)

data RcDate = RcDate {
  _whatTab :: ABRcDate,
  _dateT :: Int
                   }
instance Show RcDate where
  show dal = "RcDate " ++ (show . _whatTab) dal ++ " " ++ (show . _dateT) dal ++ "\n"
instance Eq RcDate where
  da == db = _dateT da == _dateT db

instance Ord RcDate where
  da <= db = _dateT da <= _dateT db

getCodes = getStockCodes "./module/sinaCodes"

-- company with checkRcDate
getRcRightDateL :: String -> IO [RcDate]
getRcRightDateL code = do
    bl <- getReDateLbt code
    let dbl = case isJust bl of
          True -> RcDate BonusRcDate <$> fromJust bl
          False ->  []
    al <- getReDateLat code
    let dal = case isJust al of
          True -> RcDate AllotRcDate <$> fromJust al
          False ->  []
    -- return . DL.filter (\x -> _dateT x /= 0) $ merge  dal dbl
    -- only care 2006 07 01 after
    return . DL.filter (\x -> _dateT x >= 20060701) $ merge  dal dbl
    


-- some demos of StateT for inspiration to transfer fuquan factor value between every date of price

factorT :: StateT Int IO String
factorT = StateT $ \i -> return ("Factor is " ++ show i, i + 1)

dateToFactorT :: Int -> StateT Int IO Int
dateToFactorT date = do
  sta <- get
  if odd date
    then do
    traceM $ "Factor was " ++ show sta ++ ", now updated to " ++ show date
    put date
    return date
    else do
    traceM $ "Factor was " ++ show sta ++ ", now updated to 0 "
    put 0
    return 0

-- just like sequence in plus in Main.hs, there get hidden >> operations in mapM,so StateT Int IO as monad ,could keep state
testDateL = [1,2,3,4,5,6]
testDFT = evalStateT (mapM dateToFactorT testDateL) 0
-- [1,0,3,0,5,0]

dateElem :: Int -> [RcDate] -> Bool
dateElem date rcDl = elem date $ (\x -> _dateT x) <$>  rcDl
-- another implement
dateElem2 date rcDl = DL.foldr (\a b -> b || _dateT a == date ) False rcDl

testDateElem = return . dateElem 20180712 =<< getRcRightDateL "000001"

dateIndex :: Int -> [RcDate] -> Int
dateIndex date rcDl = fromJust . DL.elemIndex date $ (\x -> _dateT x) <$>  rcDl

getClose :: String -> Int -> IO Int
getClose code date =
  fmap DL.head $ withPostgreSQL @IO pgConnectInfo $ do
  tryCreateTable stockPriceT
  query $ do
    stockL <- select stockPriceT
    restrict ( stockL ! #_code .== (literal . pack) code .&& stockL ! #_date .== literal date)
    return $ stockL ! #_close

getDividend :: String -> Int -> IO Int
getDividend code date =
  fmap DL.head $ withPostgreSQL @IO pgConnectInfo $ do
  tryCreateTable bonusInfoT
  query $ do
    stockL <- select bonusInfoT
    restrict ( stockL ! #_codeB .== (literal . pack) code .&& stockL ! #_recordDateB .== literal date)
    return $ stockL ! #_dividend

getBonusSharesRatio :: String -> Int -> IO Int
getBonusSharesRatio code date =
  fmap DL.head $ withPostgreSQL @IO pgConnectInfo $ do
  tryCreateTable bonusInfoT
  query $ do
    stockL <- select bonusInfoT
    restrict ( stockL ! #_codeB .== (literal . pack) code .&& stockL ! #_recordDateB .== literal date)
    return $ stockL ! #_bonusSharesRatio

getSharesTranscent :: String -> Int -> IO Int
getSharesTranscent code date =
  fmap DL.head $ withPostgreSQL @IO pgConnectInfo $ do
  tryCreateTable bonusInfoT
  query $ do
    stockL <- select bonusInfoT
    restrict ( stockL ! #_codeB .== (literal . pack) code .&& stockL ! #_recordDateB .== literal date)
    return $ stockL ! #_sharesTranscent

getOfferPrice :: String -> Int -> IO Int
getOfferPrice code date =
  fmap DL.head $ withPostgreSQL @IO pgConnectInfo $ do
  tryCreateTable allotmentT
  query $ do
    stockL <- select allotmentT
    restrict ( stockL ! #_codeA .== (literal . pack) code .&& stockL ! #_recordDateA .== literal date)
    return $ stockL ! #_offerPrice

getAllotRatio :: String -> Int -> IO Int
getAllotRatio code date =
  fmap DL.head $ withPostgreSQL @IO pgConnectInfo $ do
  tryCreateTable allotmentT
  query $ do
    stockL <- select allotmentT
    restrict ( stockL ! #_codeA .== (literal . pack) code .&& stockL ! #_recordDateA .== literal date)
    return $ stockL ! #_allotRatio

updateFactor :: String -> Int -> Int -> IO Int
updateFactor code date factor =
  withPostgreSQL @IO pgConnectInfo  $ do
  tryCreateTable stockPriceT
  update stockPriceT (\r -> r ! #_code .== (literal . pack) code .&&  r ! #_date .== literal date ) (\r -> r `with` [  #_factor := literal factor ])

-- need to check if all record date are in price date,for some shit stock just suspend in record day in very rare possiblity, so if not, should correct such record date to price day just next to it
-- company with getRcRightDateL
checkRcDate :: String -> [RcDate] -> IO [RcDate]
checkRcDate code rcL = do
  mapM assureRcInPrDl rcL where
    assureRcInPrDl rc = do
      prDl <- getPrDateL code
      case _dateT rc `elem` prDl of
        True -> return rc
        -- the emergence of "where" level by level represent the thought process how you concrete the process to solve the issue step by step also level by level, naming help you anchor when you are in the middle of the thinking
        False -> do
          traceM $ "got fucking suspended in Record date! code is " ++ show code ++ ",date is " ++ show rc
          rightDate <- correctRcDateOnline code (_dateT rc)
          return $ RcDate (_whatTab rc) rightDate

-- haskell get this bug, if get where embed where, the code after last where outside of the block (here is stateFactor function below) will get indent issue, so have to put last where as global function,here is correctRcDate 
correctRcDateOnline code date =
  withPostgreSQL @IO pgConnectInfo $ do
    tryCreateTable stockPriceT
    dl <- query $ do
      stockL <- select stockPriceT
      restrict $ (stockL ! #_code .== literal (pack code)) .&& (stockL ! #_date .> literal  date)
      order (stockL ! #_date) ascending
      return $ stockL ! #_date
    return $ dl !! 0

correctReDateInDB :: String -> Int -> Int -> IO Int
correctReDateInDB code date rightDate =
  withPostgreSQL @IO pgConnectInfo $ do
  tryCreateTable bonusInfoT
  update bonusInfoT (\r -> r ! #_codeB .== (literal . pack) code .&&  r ! #_recordDateB .== literal date ) (\r -> r `with` [  #_recordDateB := literal rightDate ])

correctRcDate :: String -> [RcDate] -> IO ()
correctRcDate code rcL = do
  mapM_ assureRcInPrDl rcL where
    assureRcInPrDl rc = do
      prDl <- getPrDateL code
      case _dateT rc `elem` prDl of
        True -> return ()
        -- the emergence of "where" level by level represent the thought process how you concrete the process to solve the issue step by step also level by level, naming help you anchor when you are in the middle of the thinking
        False -> do
          traceM $ "got fucking suspended in Record date! code is " ++ show code ++ ",date is " ++ show rc
          rightDate <- correctRcDateOnline code (_dateT rc)
          correctReDateInDB code (_dateT rc) rightDate
          return ()
          --rightDate <- correctRcDateOnline code (_dateT rc)
          --return $ RcDate (_whatTab rc) rightDate
                    
stateFactor :: String -> [RcDate] -> Int -> StateT Int IO () -- factor is Int
stateFactor code rcDl prDate  = do
  factor <- get
  case dateElem prDate rcDl of
    True -> do
      let dIndex = dateIndex prDate rcDl
      case _whatTab (rcDl !! dIndex) == BonusRcDate of
        True -> do
          rcClose <- liftIO $ getClose code prDate
          dividend <- liftIO $ getDividend code prDate
          bonusSharesRatio <- liftIO $ getBonusSharesRatio code prDate
          sharesTranscent <- liftIO $ getSharesTranscent code prDate
          -- all data here are in 3 digital space of accuracy but stored by *1000 in Int,so must make balance in denominator by *1000
          let rcPrice = (fromIntegral @_ @Float rcClose - fromIntegral @_ @Float dividend /10) / (1 *1000 + fromIntegral @_ @Float bonusSharesRatio/10 + fromIntegral @_ @Float sharesTranscent/10)
          let currentFactor = fromIntegral @_ @Float rcClose / rcPrice
          -- factor is in 3 digital space accuracy but stored in Int ,because here we get 2 factor multiplied to get newFactor,means we get 1000 *1000, so divide by 1000
          let newFactor =  floor $ fromIntegral @_ @Float factor * currentFactor / 1000
          liftIO $ updateFactor code prDate factor -- recordDay still use old factor
          --traceM $ "updating newFactor in BonusRcDate, code is " ++ show code ++ ",date is " ++ show prDate
          put newFactor -- next day of recordDay use new factor
        False -> do
          rcClose <- liftIO $ getClose code prDate
          allotRatio <- liftIO $ getAllotRatio code prDate
          offerPrice <- liftIO $ getOfferPrice code prDate
          -- all data here are in 3 digital space of accuracy but stored by *1000 in Int,so must make balance in denominator by *1000
          let rcPrice = (fromIntegral @_ @Float rcClose  +  fromIntegral @_ @Float offerPrice * fromIntegral @_ @Float allotRatio/10) / (1*1000 + fromIntegral @_ @Float allotRatio/10 )
          let currentFactor = fromIntegral @_ @Float rcClose / rcPrice
          -- factor is in 3 digital space accuracy but stored in Int ,because here we get 2 factor multiplied to get newFactor,means we get 1000 *1000, so divide by 1000
          let newFactor =  floor $ fromIntegral @_ @Float factor * currentFactor / 1000
          liftIO $ updateFactor code prDate factor  -- recordDay still use old factor
          --traceM $ "updating newFactor in AllotRcDate, code is " ++ show code ++ ",date is " ++ show prDate
          put newFactor -- next day of recordDay use new factor

    False -> do
      liftIO $ updateFactor code prDate factor
      return ()
      --traceM $ "updating with oldFactor, code is " ++ show code ++ ",date is " ++ show prDate


testRcL = [RcDate BonusRcDate 20180822
          ,RcDate BonusRcDate 20190814
          ,RcDate BonusRcDate 20200813]

-- price list must get same date coverage with Rc right list,or else get empty list error
-- online update rcl make head empty list exception when getDividend,because rc date is correctified online,but when getDividend,rc date is still got from database,which is not correctified
-- how to debug unknown exception !!
-- *Main> :set -fbreak-on-exception
-- *Main> :set args "DataBase.hs"
-- *Main> :trace testStateFactor
-- ...
-- Stopped in <exception thrown>, <unknown>
-- _exception :: e = _
--  [<unknown>] lambda :back
-- Logged breakpoint at /home/kyle/Haskell/SpidingShSz/module/DataBase.hs:719:59-89
-- _result :: Float
-- dividend :: Int
-- [-1: /home/kyle/Haskell/SpidingShSz/module/DataBase.hs:719:59-89] lambda :list
-- 718            -- all data here are in 3 digital space of accuracy but stored by *1000 in Int,so must make balance in denominator by *1000
-- 719            let rcPrice = (fromIntegral @_ @Float rcClose - fromIntegral @_ @Float dividend /10) / (1 *1000 + fromIntegral @_ @Float bonusSharesRatio/10 + fromIntegral @_ @Float sharesTranscent/10)
--                                                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- 720            let currentFactor = fromIntegral @_ @Float rcClose / rcPrice

testStateFactor = do
  -- now we get only data to 2017,so not use rcL,but use testRcL for test
  rcL <- getRcRightDateL "000001" >>= checkRcDate "000001"
  prDl <- getPrDateL "000001"
  -- factor is in 3 digital space accuracy but stored in Int ,so initial state is multiplied 1000
  execStateT (mapM (stateFactor "000001" rcL) prDl) (1*1000)
  -- finally return latest factor value
  
updateAllFactors :: String -> IO ()
updateAllFactors code = do
  -- rcL <- getRcRightDateL code >>= checkRcDate code
  getRcRightDateL code >>= correctRcDate code
  rcL<-getRcRightDateL code
  prDl <- getPrDateL code
  execStateT (mapM (stateFactor code rcL) prDl) (1*1000)
  return ()

-- self.cursor.execute(
--             'update stock set fuquan_average=average*factor/%f where code="%s" '
--             % (self.latestFactor, self.stockCode)
--         )

getLatestFactor :: String -> IO Int
getLatestFactor code = do
  pgCon <- pgOpen pgConnectInfo
  withPostgreSQL @IO pgConnectInfo $ do
    tryCreateTable stockPriceT
    factorL <- query $ do
      stockL <- select stockPriceT
      restrict (stockL ! #_code .== literal (pack code))
      order (stockL ! #_date) descending
      return $ stockL ! #_factor
    return $ factorL !! 0
  

updateExPrices :: String -> IO Int
updateExPrices code = do
  latestFactor <- getLatestFactor code
  withPostgreSQL @IO pgConnectInfo $ do
    tryCreateTable stockPriceT
    update stockPriceT (\r -> r ! #_code .== (literal . pack) code ) (\r -> r `with` [  #_fuquan_average := fromInt ( r ! #_average) * fromInt (r ! #_factor) / fromInt (literal latestFactor) ])

logOutD log = (>>) <$> liftIO . (appendFile log) <*> traceM
-- updateFactorAndExPrices "./module/sinaCodes"
updateFactorAndExPrices :: FilePath -> IO ()
updateFactorAndExPrices fp = do
  let threadList = [1..30]
  codeL <- getStockCodes fp
  mCodeL <- newMVar codeL -- ["603955"] --codeL
  syncM <- newEmptyMVar
  --mapM_ updateAllFactors codeL
  --mapM_ updateExPrices codeL
  mapM (forkIO . threadUpdateFactor syncM mCodeL) threadList
  mapM_ (\_ -> takeMVar syncM)  threadList

  --mapM (forkIO . threadUpdateUpdateExPrice syncM mCodeL) threadList
  --mapM_ (\_ -> takeMVar syncM)  threadList

  --takeMVar syncM
  --takeMVar syncM
  traceM "finished!"
  return ()

threadUpdateFactor syncM  mCodeL thread = do
  --updateAllFactors codeL
  id <- myThreadId
  --let log = "./log/" ++ ((!! 1) . (DL.words) . show) id ++ "log.txt"
  codeL <- takeMVar mCodeL
  case (DL.null) codeL of
    True -> do
      traceM $ show id ++  " out for codeL is empty \n"
      putMVar mCodeL codeL
      putMVar syncM 'x'
    False -> do
      let code = DL.head codeL
      traceM $ show id ++  " updateAllFactors of " ++ show code ++ "\n"
      putMVar mCodeL (DL.tail $ codeL)
      updateAllFactors code
      threadUpdateFactor syncM  mCodeL thread

threadUpdateUpdateExPrice syncM  mCodeL thread = do
  --updateAllFactors codeL
  id <- myThreadId
  --let log = "./log/" ++ ((!! 1) . (DL.words) . show) id ++ "log.txt"
  codeL <- takeMVar mCodeL
  case (DL.null) codeL of
    True -> do
      traceM $ show id ++  " out for codeL is empty \n"
      putMVar mCodeL codeL
      putMVar syncM 'x'
    False -> do
      let code = DL.head codeL
      traceM $ show id ++  " threadUpdateUpdateExPrice of " ++ show code ++ "\n"
      putMVar mCodeL (DL.tail $ codeL)
      updateExPrices code
      threadUpdateUpdateExPrice syncM  mCodeL thread
      
