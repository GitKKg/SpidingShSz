-- | 
{-# LANGUAGE OverloadedStrings #-}
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

-- http://money.finance.sina.com.cn/corp/go.php/vISSUE_ShareBonus/stockid/002166.phtml
sinaURL :: String ->String
sinaURL code = "http://money.finance.sina.com.cn/corp/go.php/vISSUE_ShareBonus/stockid/"
  ++ code ++ ".phtml"

startDate = 2003 -- season 01

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

getData :: String -> IO ()
getData code = do
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
  traverse print . fromJust $ scrapeStringLike gbkPage stockScraper
  return ()
  where
    stockScraper :: Scraper L8.ByteString [RightInfo]
    stockScraper =
      (<|>) <$> (onePageDataB =<<)  <*> (onePageDataA =<<) $  getName  where
      getName = text stockName :: Scraper L8.ByteString L8.ByteString
      onePageDataB :: L8.ByteString -> Scraper L8.ByteString [RightInfo]
      onePageDataB = (fmap.fmap) Left . chroots  bonusRow  . oneDayScraper  where
        oneDayScraper name = undefined
      onePageDataA :: L8.ByteString -> Scraper L8.ByteString [RightInfo]
      onePageDataA = (fmap . fmap) Right . chroots allotmentRow  . oneDayScraper  where
        oneDayScraper = undefined
      -- getName >>= onePageData where
      -- getName = text stockName :: Scraper L8.ByteString L8.ByteString
      -- onePageData = chroots  bonusRow  . oneDayScraper  where
      --   oneDayScraper = undefined
  
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
tt = (,) <$> (+2) <*> (+3) $ 12
ttt = (,,) <$> (+1) <*> (+2) <*> (+3) $ 12
-- (12+1,12+2,12+3)
addSame = (plus3) <$> (+1) <*> (($2). (-) ) <*> (*3) $ 8
--(8+1) + (8-2) + (8*3) == 39
plus3 = \x y z -> x+y+z
