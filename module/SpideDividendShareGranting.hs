-- | 
{-# LANGUAGE OverloadedStrings,TypeApplications #-}
module SpideDividendShareGranting where

import Text.HTML.Scalpel
import Control.Applicative

import Network.HTTP.Client

import qualified Data.ByteString.Lazy.Char8 as L8

--import           Network.HTTP.Simple

import Text.HTML.TagSoup

import Network.Connection
import Network.Socket
import Network.HTTP.Client.TLS

import Network.HTTP.Types.Status (statusCode)
-- Scraper is just like a converting format  from some input to output
-- Selector is just like a selecting format to select DOM node
-- chroot accept an old Scraper and output a new Scraper,because it add a range by Selector for old Scraper

import Control.Exception
import Data.Either
import Data.Typeable

import Network.HTTP.Types.Header
import Data.CaseInsensitive

import Data.Text.Encoding

import Data.Maybe

--import Db -- Db.hs in the same directory,could import directly

import Text.Regex.Base.RegexLike
import Text.Regex.Posix.String

import Data.Functor

import Data.Text


import qualified Data.Text.Encoding as T       -- text
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.ICU.Convert as ICU  -- text-icu,proxychains stack install text-icu
import qualified Data.Text.ICU as ICU

import DataBase

import Control.Monad
import Debug.Trace

import Text.Read

import Control.Concurrent

-- http://money.finance.sina.com.cn/corp/go.php/vISSUE_ShareBonus/stockid/002166.phtml
sinaURL :: String ->String
sinaURL code = "http://money.finance.sina.com.cn/corp/go.php/vISSUE_ShareBonus/stockid/"
  ++ code ++ ".phtml"

startDate = 2003 -- season 01

tlsSetting = TLSSettingsSimple False False False -- import Network.Connection
hostAddr = "127.0.0.1" :: HostName -- import Network.Socket
-- using 192.168.1.2 will Exception as ConnectionTimeout
portNumber = 5678 :: PortNumber
proxySetting = SockSettingsSimple hostAddr portNumber

-- note: for per stock code,we need:
-- 1. year,season,month,day of current day spidered
-- 2. latest year,season,month,day of data of database
-- 3. oldest year,season,month,day of data of database
-- plan since 2003.1.1 to now
-- one fact , before getting new data ,can't calulate latest verion right recovering factors  of old data just with right recovering info
-- 4. so, must get latest date of current spiding pricing page if year and season of it is newest,ie. even newer than in database
-- 5. In spiding price page,date order in decend, ie. new date is on top of old one,newer date is got at first
-- 6. so when dispatch spiding price pages and spding dividend right pages between stock codes and seasons to multi-threads pool, just only can in stock codes, to guarntee spiding from new to old in same stock codes only in one same thread
 
stockName :: Selector
stockName = "div" @: [hasClass "toolbartop",AttributeString "id" @= "toolbar"] // "div" @:[hasClass "tbtb01"] `atDepth` 1 // "h1" `atDepth` 1

getFrSinaDemo :: IO ()
getFrSinaDemo = do
  systemManager <- newManager tlsManagerSettings
  requestSinaNoHead <- parseRequest $ sinaURL "000002"
  responseSinaNoHead <- httpLbs requestSinaNoHead systemManager
  putStrLn $ "The Bing status code was: " ++ (show $ statusCode $ responseStatus responseSinaNoHead)
  gbk <- ICU.open "gbk" Nothing
  let txt :: Text
      txt = ICU.toUnicode gbk $ L8.toStrict $ responseBody responseSinaNoHead
  -- T.putStrLn txt
  -- 万科A
  let gbkPage = L8.fromStrict . encodeUtf8 $ txt
  L8.putStrLn.fromJust $ scrapeStringLike gbkPage ( text stockName)
  L8.putStrLn.fromJust $ scrapeStringLike gbkPage (attr "id" bonusTab) -- sharebonus_1
  L8.putStrLn.fromJust $ scrapeStringLike gbkPage (html bonusRow)
  print "now show dividend table"
  traverse L8.putStrLn . fromJust $ scrapeStringLike  gbkPage (texts bonusRow)
  print "now show allotment table"
  traverse L8.putStrLn . fromJust $ scrapeStringLike  gbkPage (texts allotmentRow)
  print "over"

bonusTab :: Selector
bonusTab = "table" @: [AttributeString "id" @= "sharebonus_1"] -- hasClass "table",note,page source has  no table this class attribute,but f12 has

bonusRow :: Selector
bonusRow = bonusTab // "tbody" `atDepth` 1 // "tr" `atDepth` 1

allotmentTab :: Selector
allotmentTab = "table" @: [AttributeString "id" @= "sharebonus_2"]

allotmentRow :: Selector
allotmentRow = allotmentTab // "tbody" `atDepth` 1 // "tr" `atDepth` 1

-- <|> of Scraper seems just discard one operand,so must implement <> to collect two scrapers results
instance Semigroup a => Semigroup (Scraper r a) where
  sa <> sb = fmap (<>) sa  <*> sb

onePageRight :: Maybe PortNumber -> String -> IO [RightInfo]
onePageRight mp code = do
  --let v2managerSetting = mkManagerSettings tlsSetting (Just proxySetting)
  systemManager <- newManager $
    if isJust mp
    then mkManagerSettings tlsSetting (Just $ SockSettingsSimple hostAddr (fromJust mp))
    else tlsManagerSettings
    
  requestSinaNoHead <- parseRequest $ sinaURL code
  let getpage = do
        -- TypeApplications make you type less words, use @ !
        eResponse <- try @SomeException  $ httpLbs requestSinaNoHead systemManager
        case eResponse of
          Left e -> do
            traceM $ "Sina exception! is \n" ++ show e ++ "\nstop onePageData!"
            traceM $ "wait for 1s,repeat again \n"
            threadDelay 1000000
            getpage -- mzero make you exit onePageData in advance
          Right response ->  return response
          
  responseSinaNoHead <- getpage
  
  putStrLn $ "The Sina status code was: " ++ (show $ statusCode $ responseStatus responseSinaNoHead)
  gbk <- ICU.open "gbk" Nothing
  let txt :: Text
      txt = ICU.toUnicode gbk $ L8.toStrict $ responseBody responseSinaNoHead
  -- T.putStrLn txt
  -- 万科A
  let gbkPage = L8.fromStrict . encodeUtf8 $ txt
  traverse print . fromJust $ scrapeStringLike gbkPage stockScraper
  traverse return . fromJust $ scrapeStringLike gbkPage stockScraper
  
  -- new topic, how to parallelly pass 2 parameters inside monad into 2 monad
  -- first to figure out how to pass 2 parameters into 1 monad:
  -- ma = return 1 :: IO Integer
  -- mb = return 2 :: IO Integer
  -- mminus = (return :: a -> IO a) (-)
  -- mresult = mminus <*> ma <*> mb -- -1
  where
    floorFloatToInt = (floor :: Float -> Int) . (1000 *)
    removeComma :: String -> String -- in case we get 120,500.56 such great number
    removeComma = unpack . mconcat . splitOn (pack ",") . pack
    stockScraper :: Scraper L8.ByteString [RightInfo]
    stockScraper = -- use prototypes of mmParalell2Para to paralell parameters with functions in monad
      -- change <|> to <>,for <|> just discard onePageDataA's result
      (<>) <$> ( ($ onePageDataB) =<<)  <*> ( ($ onePageDataA) =<<)
      $ fmap (.) (return ($ code)) <*> (fmap (flip ($)) getName)  where
      getName = text stockName :: Scraper L8.ByteString L8.ByteString
      -- Bonus Table
      onePageDataB :: L8.ByteString -> String -> Scraper L8.ByteString [RightInfo]
      onePageDataB = (fmap.fmap) ((fmap.fmap) Left . chroots  bonusRow)   oneDayScraperB  where
        oneDayScraperB name code  = do
          inSerial $ do
            --traceM $ "name is " ++ (T.unpack .decodeUtf8.L8.toStrict)  name
            announceDateB <-  (read :: String -> Int) . dateToNum . L8.unpack  <$> (seekNext $ text "td")
            --traceM $ "announeDateB is " ++ show announceDateB
            bonusSharesRatio <- floorFloatToInt . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td")
            --traceM $ "bonusSharesRatio is " ++ show bonusSharesRatio
            sharesTranscent <- floorFloatToInt . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td")
            dividend <- floorFloatToInt . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td")
            process <-  decodeUtf8 . L8.toStrict <$> (seekNext $ text "td")
            --traceM $ "process is " ++ T.unpack process
            --traceM $ "process is 实施 is " ++ show (T.unpack process == "实施")
            exRightDateB <- -- don't think do while in monad do sugar,all these name on left of <- are just parameters in every layer of monad >>= functions chain
              if (T.unpack process == "实施")
              then  readCaus <$> (seekNext $ text "td")
              else  return 0
            recordDateB <-
              if (T.unpack process == "实施")
              then  readCaus  <$> (seekNext $ text "td")
              else  return 0
            -- use (,) to package parameters to accept if else logic together,but do we know (a,b), a or b which one is valued first?
            -- (exRightDateB,recordDateB) <-
            --   if (process == "实施")
            --   then return ((read :: String -> Int) . dateToNum . L8.unpack  <$> (seekNext $ text "td"),
            --                (read :: String -> Int) . dateToNum . L8.unpack  <$> (seekNext $ text "td")) -- betweeb two seekNexts ,get no monad >>= relationship,so not contionus,not you want
            --   else return (return 0,return 0)
              
            --traceM $ "exRightDateB is " ++ show  exRightDateB
            --traceM $ "recordDateB is " ++ show recordDateB
            --traceM $ "B Table,Stock name is " ++ (T.unpack .decodeUtf8.L8.toStrict)  name
            return defaultBonusInfo {
              _codeB = pack code,
              _nameB = decodeUtf8 . L8.toStrict $ name,
              _announceDateB = announceDateB, 
              _bonusSharesRatio = bonusSharesRatio,
              _dividend = dividend,
              _process = T.unpack process == "实施",
              _exRightDateB = exRightDateB,
              _recordDateB = recordDateB
                                    }
               -- let recordDateB = floorFloatToInt . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td") >>=
      -- Allotment Table
      onePageDataA :: L8.ByteString -> String ->Scraper L8.ByteString [RightInfo]
      onePageDataA  = (fmap.fmap) ((fmap . fmap) Right . chroots allotmentRow)  oneDayScraperA  where
        oneDayScraperA name code = do
          inSerial $ do
            --announeDateA <-  (read :: String -> Int) . dateToNum . L8.unpack  <$> (seekNext $ text "td")
            --traceM $ "now onepageDataA name is " ++ (T.unpack .decodeUtf8.L8.toStrict)  name
            --test <- seekNext $ text "td"
            --traceM $ L8.unpack test
            traceM "hi!exception for 600002"
            --test <-seekNext $ text "td"
            --traceM $ "text is " ++ (show . T.unpack .decodeUtf8.L8.toStrict) test
            -- many stocks A table is just empty,so must take care,using guard to exit in case
            announceDateA <- readCaus  <$> (seekNext $ text "td")
            guard (announceDateA /= 0)
            
            --traceM $ "announeDateA is " ++ show announceDateA
            allotRatio <- floorFloatToInt . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td")
            --traceM $ "allotRatio is " ++ show allotRatio
            offerPrice <- floorFloatToInt . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td")
            capStock <- floorFloatToInt . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td")
            exRightDateA <- readCaus  <$> (seekNext $ text "td")
            recordDateA <- readCaus  <$> (seekNext $ text "td")
            --traceM $ "A Table,Stock name is " ++ (T.unpack .decodeUtf8.L8.toStrict)  name
            return defaultAllotmentInfo {
              _codeA = pack code,
              _nameA = decodeUtf8 . L8.toStrict $ name,
              _announceDateA = announceDateA, 
              _allotRatio = allotRatio,
              _offerPrice = offerPrice,
              _capStock = capStock,
              _exRightDateA = exRightDateA,
              _recordDateA = recordDateA
                                        }
      -- getName >>= onePageData where
      -- getName = text stockName :: Scraper L8.ByteString L8.ByteString
      -- onePageData = chroots  bonusRow  . oneDayScraper  where
      --   oneDayScraper = undefined
readCaus  = (\res -> if res == Nothing then 0 else fromJust res). (readMaybe :: String -> Maybe Int) . dateToNum . L8.unpack -- sometime we just get "--" but not digital,so must handle in case
dateToNum :: String -> String
dateToNum = unpack . mconcat . splitOn (pack "-") . pack  -- 2020-01-01 to 20200101
-- some paralell mechanism in semantics of (->),ie. fucntion
f = (+10)
h = (+12)
g = ($ 2)
p = ($) <$> (f .)  <*> g $ h
-- <$>,<*> are both infixl 4, so associated from left to right
-- to function,<$> means pipe,ie.,apply another fucntion f on its output
-- to function, f1 <*> f2 means ,f1 ,f2 's first parameters get same type,so apply f1' rest part(as function) on f2's rest part (as data)
-- all below are just talked about type variation, but semantics under type limit is inherent,could not be infered from type clarfication
-- so , in general ,12 as parameter,both applied by (+2) and (+3) ,and then two output results are applied by (,),this is a paralell semantics of function
-- or ,we say this inherent feature must be implenmted by instance Functor and Applicative of  operator (->)
-- ref to p815 of FirstPrinciples:
-- instance Applicative ((->) r) where
--   pure = const
--   f <*> a = \r -> f r (a r) -- see, one r ,same r is applied twice, by f and a,different function
-- according to instance implenmented above, could get inference below:
-- g <$> f <*> a = \r -> (g . f) r (a r) ,or say , g <$> f <*> a = \r -> g (f r) (a r)

tt = (,) <$> (+2) <*> (+3) $ 12
ttt = (,,) <$> (+1) <*> (+2) <*> (+3) $ 12
-- (12+1,12+2,12+3)
addSame = (plus3) <$> (+1) <*> (($2). (-) ) <*> (*3) $ 8
--(8+1) + (8-2) + (8*3) == 39
plus3 = \x y z -> x+y+z

-- double passivce voices , operator and operand swap its role and position each other to achieve
-- two parammeter parallell with two functions with same types
paralell2Para = (,) <$> ($ (+)) <*> ($ (-)) $ (($ 2) . ($ 3)) 
--(5,1)

mp2 = (return :: a -> IO a) ($ 2)
mp3 = (return :: a -> IO a) ($ 3)
mPlus = (return :: a -> IO a) ($ (+))
mMinus = (return :: a -> IO a) ($ (-))
-- same protype run in monad, maybe useful to above scraper
mParalell2Para = (,) <$> (mPlus <*>) <*> (mMinus <*>) $ fmap (.) mp2 <*> mp3
-- fst mParalell2Para
-- 5
-- snd mParalell2Para
-- 1

-- protype could be used by onePageData above, two parameters paralell with two functions which get same type,but not in monad, but geneate monad
omPlus = (fmap.fmap) (return :: a -> IO a)  (+)
omMinus = (fmap.fmap) (return :: a -> IO a)  (-)
pMoPlus = ($ omPlus)
pMoMinus = ($ omMinus)
mmParalell2Para = (,) <$> (pMoPlus =<<) <*> (pMoMinus =<<) $ fmap (.) mp2 <*> mp3
-- still (5,1)
