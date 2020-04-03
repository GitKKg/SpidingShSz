{-# LANGUAGE OverloadedStrings #-}

module SpidePrice
    ( someFunc,
      printStockName,
      stock163URL,
      onePageData
    ) where

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

import Debug.Trace

import DataBase

someFunc :: IO ()
someFunc = putStrLn "someFunc"

stock163URL :: String -> Int -> Int -> String
stock163URL stockCode year season =
  "http://quotes.money.163.com/trade/lsjysj_" ++ stockCode ++ ".html?year="
  ++ show year ++ "&season="++ show season

stockName :: Selector
stockName = "h1" @: [AttributeString "class" @= "title_01"]  // "span" @: [ hasClass "name"] `atDepth` 1

stockTab :: Selector
stockTab =  "div" @: [hasClass "inner_box"] // "table" @:[AttributeString "class" @= "table_bg001 border_box limit_sale"] `atDepth` 1

--  ^ means match from start, $ means match end, ^$ means mathc null
-- honestly, regexLike relevant doc is pile of shit
data1OrData2 = makeRegex ("^$|dbrow" :: String) :: Regex -- must specify type notation "String" or else complier will complain

onePageData :: String -> Int -> Int -> IO () --"000001" 2020 01
onePageData stockCode year season = do
  systemManager <- newManager tlsManagerSettings
  request163NoHead <- parseRequest $ stock163URL stockCode year season
  response163NoHead <- httpLbs request163NoHead systemManager
  traverse print . fromJust $ scrapeStringLike (responseBody response163NoHead)  stockScraper
  return ()
  where
    stockScraper :: Scraper L8.ByteString [Stock]
    stockScraper = 
      -- must use decodeUtf8 making Text show Chinese correcttly ,accompany with unpack in show instance
      --fmap (\day -> day {_name = decodeUtf8 . L8.toStrict $  name}) <$> onePageData
      getName >>= onePageData  where
        getName = text stockName ::Scraper L8.ByteString L8.ByteString
        onePageData  = chroots (stockTab // "tr" @: [AttributeString "class" @=~ data1OrData2] `atDepth` 1)  . oneDayScraper  where
          oneDayScraper :: L8.ByteString -> Scraper L8.ByteString Stock
          oneDayScraper name  = do
            --name <- text stockName
            inSerial $ do
              --date <-  (pack . L8.unpack) <$> (seekNext $ text "td")
              date <-  (read :: String -> Int) . dateToNum . L8.unpack  <$> (seekNext $ text "td")
-- use trace to print debug info inside a monad which have no monadtrans and no exporting value structor 
              trace ("date is " ++ show date) $ return date
              open <- floorFloatToInt . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td") 
              high <- floorFloatToInt . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td")
              low <-  floorFloatToInt . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td")
              close <- floorFloatToInt . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td")
              _ <- seekNext $ text "td"
              _ <- seekNext $ text "td"
              -- read with float in case getting weird non-integer numeber,not likely,but heard ever
              shares <- (floor :: Float -> Int) . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td") -- in 100, one hand stock
              value <- (floor :: Float -> Int) . (read :: String -> Float) .removeComma . L8.unpack <$> (seekNext $ text "td") -- in 10 thousnad RMB Yuan
              return $ defaultStock {
                _date = date,
                _shares = shares,
                _open = open,
                _high = high,
                _close = close,
                _low = low,
                _value = value,
                -- must use decodeUtf8 making Text show Chinese correcttly ,accompany with unpack in show instance
                -- another way to write name inside,suitble for scenario where some fields depend each other 
                _name = ( ( $ pack ")") . append ) .  (Prelude.head)  $ split (\c -> c==')') . decodeUtf8 . L8.toStrict $ name
                                    } where
                floorFloatToInt = (floor :: Float -> Int) . (1000 *) 
                dateToNum :: String -> String
                dateToNum = unpack . mconcat . splitOn (pack "-") . pack  -- 2020-01-01 to 20200101
                removeComma :: String -> String -- in case we get 120,500.56 such great number
                removeComma = unpack . mconcat . splitOn (pack ",") . pack
  

--printAllStock :: IO ()
-- printAllStock = onePageData >>= traverse (putStr .show .setName ) . fromJust >> putStr "\nover\n" where
--   setName name = do
--     --return $ (\oneDay -> oneDay {_name = pack . L8.unpack $ name}) <$> stockA
--     gbk <- ICU.open "gbk" Nothing
--     let txt = ICU.toUnicode gbk $ L8.toStrict name
--     return $ (\oneDay -> oneDay {_name =  txt}) <$> stockA
    

printStockName = T.putStrLn . ( ( $  pack ")") . append  ) .  (Prelude.head)  $ split (\c -> c==')') $ pack "浦发银行(600000) 历史交易数据"

printStockNameW2 = T.putStrLn . (  flip append  (pack ")") ) .  (Prelude.head)  $ split (\c -> c==')') $ pack "浦发银行(600000) 历史交易数据"
