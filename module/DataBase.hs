-- | 
{-#  LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels,DuplicateRecordFields #-}

module DataBase (
  -- Stock (Stock) will just export constructor, no field fucntion like _code,_date
    Stock (..), -- export everything in Stock definition
    defaultStock
) where

import Database.Selda   --proxychains stack install selda-0.4.0.0
import Database.Selda.SQLite
import Control.Exception
import Data.Typeable

import Data.Text


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
    "\n_code =" ++ (unpack . _code $ stock) ++
    ",\n_date =" ++ (show . _date $ stock) ++
    -- must use unpack, using show can't show Chinese 
    ",\n_name =" ++ (unpack . _name $ stock) ++ 
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
