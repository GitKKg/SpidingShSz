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
import qualified Data.Text.ICU.Convert as ICU  -- text-icu   ,proxychains stack install text-icu
import qualified Data.Text.ICU as ICU

import DataBase

-- http://money.finance.sina.com.cn/corp/go.php/vISSUE_ShareBonus/stockid/002166.phtml
sinaURL :: String ->String
sinaURL code = "http://money.finance.sina.com.cn/corp/go.php/vISSUE_ShareBonus/stockid/"
  ++ code ++ ".phtml"

-- note: for per stock code,we need:
-- 1. year,season,month,day of current day spidered
-- 2. latest year,season,month,day of data of database
-- 3. oldest year,season,month,day of data of database
-- plan since 2003.1.1 to now
-- one fact , before getting new data ,can't calulate latest verion right recovering factors  of old data just with right recovering info
