{-# LANGUAGE OverloadedStrings ,TypeApplications #-}

module SpidePrice
    (
      onePagePrice
    )
where

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

import Control.Monad
import Data.Functor

import Control.Error -- from the 'errors' package
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either
--exit = mzero

import Control.Concurrent

import DebugLogM

import System.Clock

-- Either is monad, and >> chain of Either will break at the 1st Left,so could use this feature to exit in advance
-- ExceptT too, as monad transformer, could use lift,lifIO to bring try ,catch such exception io inside,so as to wrap your routine will induce exception in ExcepT monad chain >> block, make Except monad block exit at once when encounter first exception you specifying, and you get return calue inside Left,if no exception at all, you get return value in Right
-- MaybeT is similar,but just return Nothing when exit due to exception,return Just xx when finished normally

mayForEver = runMaybeT $ forever $  do
    str <- lift getLine
    when (str == "exit") $ liftIO (print "exit") >> mzero
    logOutM $ "continue\n"

ex2 = runExceptT $ forever $ do
  str <- lift getLine
  when (str == "exit") $ left 1 -- only left will make loop exit, but right will continue loop
  liftIO $ print "continue\n"
    


tlsSetting = TLSSettingsSimple False False False -- import Network.Connection
hostAddr = "127.0.0.1" :: HostName -- import Network.Socket
-- using 192.168.1.2 will Exception as ConnectionTimeout
portNumber = 5678 :: PortNumber
proxySetting = SockSettingsSimple hostAddr portNumber

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

-- proxy port, stock code, year, season
onePagePrice :: MVar [(String,Int,Int)] -> Maybe PortNumber -> Int -> String -> Int -> Int -> IO (Int,[Stock]) --"000001" 2020 01
onePagePrice mlist mp howManyPort stockCode year season = do
  --let v2managerSetting = mkManagerSettings tlsSetting (Just proxySetting)
  logOutM $ "onePagePrice,Stock code is " ++ stockCode ++ ",port is " ++ show mp ++ "\n"
  systemManager <- newManager $
    if isJust mp
    then mkManagerSettings tlsSetting (Just $ SockSettingsSimple hostAddr (fromJust mp))
    else tlsManagerSettings
  
  request163NoHead <- parseRequest $ stock163URL stockCode year season
  let getpage mlist mp howManyPort stockCode year season= do
        -- TypeApplications make you type less words, use @ !
        eResponse <- try @SomeException  $ httpLbs request163NoHead systemManager
        case eResponse of
          Left e -> do -- it's timeout in most cases
            logOutM $ "exception! is \n" ++ show e ++ "\nstop onePageData!\n"
            logOutM $ "wait for 1s,repeat again \n"
            threadDelay 1000000
            -- use case not if for if and do embed logic,if will cause many indent issues can't be resloved
            case (isJust mp) of
              True -> do
                list <-takeMVar mlist
                case (Prelude.length list < howManyPort) of
                  True -> do
                    putMVar mlist $ [(stockCode,year,season)] ++ list -- let direct link to finish the rest
                    --putMVar syncM 'x'
                    mzero
                  otherwise -> putMVar mlist list
              otherwise -> return ()
            getpage mlist mp howManyPort stockCode year season -- mzero make you exit onePageData in advance
          Right response ->  return response
          
  response163NoHead <- getpage mlist mp howManyPort stockCode year season
  endT <- getTime Monotonic
  let endSec = round . (/ (10^9) ) . fromInteger . toNanoSecs $ endT
  logOutM $ "get Pages!\n"
  -- be hold! sometime will get invalid data ,need handling  
  -- Exception: Maybe.fromJust: Nothing
  eStock <- try @SomeException $traverse return . fromJust $ scrapeStringLike (responseBody response163NoHead)  stockScraper
  case eStock of
          Left e -> do
            logOutM $ "exception when parsing! is \n" ++ show e ++ "\n it seems baned by 163!\n"
            logOutM $ "wait for 5 mins,repeat again \n"
            threadDelay $ 1000000*60*5
            onePagePrice mlist mp howManyPort stockCode year season 
          Right stock ->  return (endSec, stock)
  --print stockA
  --return stockA
  where
    stockScraper :: Scraper L8.ByteString [Stock]
    stockScraper = 
      -- must use decodeUtf8 making Text show Chinese correcttly ,accompany with unpack in show instance
      --fmap (\day -> day {_name = decodeUtf8 . L8.toStrict $  name}) <$> onePageData
      join $ onePageData <$> getName <*> return stockCode where
        getName = text stockName ::Scraper L8.ByteString L8.ByteString
        onePageData :: L8.ByteString  -> String -> Scraper L8.ByteString [Stock]
        -- fmap . fmap here is compose function which get two parameters,here is oneDayScraper
        -- (.) . (.) is ok too
        onePageData  = (fmap . fmap) (chroots (stockTab // "tr" @: [AttributeString "class" @=~ data1OrData2] `atDepth` 1))  oneDayScraper  where
          oneDayScraper :: L8.ByteString -> String -> Scraper L8.ByteString Stock
          oneDayScraper name code  = do
            --name <- text stockName
            inSerial $ do
              --date <-  (pack . L8.unpack) <$> (seekNext $ text "td")
              date <-  (read :: String -> Int) . dateToNum . L8.unpack  <$> (seekNext $ text "td")
-- use trace to print debug info inside a monad which have no monadtrans and no exporting value structor 
              -- trace ("date is " ++ show date) $ return date
              -- traceM is more simple
              -- traceM $ "date is " ++ show date
              
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
                _code = pack code,
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


-- some continuous passive voices example,used ever above:
pa = ($ 1)
pb = ($ 2)
ps = pb . pa $ (-)  -- pa (-) get 2-, pb .pa $ (-) get 2-3 = -1
-- continous passive voices in monad
mpa = (return :: a -> IO a) ($ 1)
mpc = (pb .) <$> mpa
mmplus = (return :: a -> IO a) (+)
mps =  mpc <*> mmplus
mpb = (return :: a -> IO a) (($ 2) .)
mmps = mpb <*> mpa <*> mmplus
-- still 3
bmplus = (return :: a -> IO a) . (+) -- look this as onePageData
inmmps = mpa <*> ( (return 2) >>= bmplus)
-- still 3
-- note! $ 2 is different with ($) 2, so we can't fmap $ into functor, only can fmap ($)
-- so ,we can't make data inside functor become passive!shame!
-- actually,we can:
-- note: $ is infixr operator,$ of ($ 2) is infixr operator,too,but ($) is function operator
-- so you can use fmap (flip ($)) to make inside functor become passive!

mbs = (return :: a -> IO a) "hi"
--mpbc :: Num p => IO ((a -> p -> c )->a ->c)
mpbs =  flip ($) mbs
mpbsc = (.) . flip ($) <$>  mbs  -- (.) is infixr

pas = ($ "ok")
--mpab :: Num a =>  IO ((a -> a -> c) -> c)
mpab = mpbsc <*> (return :: a -> IO a) pas

--mminus :: Num a => a -> a -> IO a
smminus = ($ (fmap . fmap) (return :: a -> IO a)  (++)) -- not pass if put here,weird, mminus :: Num a => a -> a -> IO a , maybe compiler need to know type signatur first here,like c declare
--pm :: IO [Char]
pm =  ($ (++) ) <$> mpab -- "okhi"

aminus = join $ smminus <$> mpab

mn3 = (return :: a -> IO a) (3 :: Num p => p) -- it's not work: return 3 :: Num p => p -> IO p
mpn3 = flip ($) mn3
mcpn3 = (.) . flip ($) <$> mn3 -- continuous passive version
pn2 = ($ 2) 
mpn2n3 = mcpn3 <*> (return :: a -> IO a) pn2
mnpminus = ($ (fmap . fmap) (return :: a -> IO a) (-))
mncal = join $ mnpminus <$> mpn2n3
-- 2-3 = -1
