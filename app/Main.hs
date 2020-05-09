{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main (
  main
            ) where
import DataBase
import SpidePrice
import SpideDividendShareGranting
import Data.Time.Clock
import Data.Time.Calendar
import Data.Tuple.Extra
-- use magit to create repository on github:
-- H(Magithub) c(Create on GitHub)
-- M(Remote) a(Add) : specify remote name and this new repository address in github
-- (i.e. https://github.com/GitKKg/SpidingShSz)

import Control.Monad

import Control.Concurrent

import Data.Either

main :: IO ()
main = someFunc

type StartYear = Int
type StartSeason = Int
type EndYear = Int
type EndSeason = Int
type Year = Int
type Season = Int

-- newtype Season  = Season_ Int
-- so called smart constructor
-- season :: Int -> Season
-- season a
--   | a>=1 && a<=4 = Season_ a
--   | otherwise = error "no such season!"

-- seasonNum :: Season -> Int
-- seasonNum (Season_ a) = a

-- actually smart constructor still take effect in runtime, so why not check whole parameters in program

-- filter  (not .(\ (s,y) -> s==2017&&y<3 || s==2020&&y>2  )) sl

getYSList :: StartYear -> StartSeason -> EndYear -> EndSeason -> IO [(Year,Season)] 
getYSList y1 s1 y2 s2 = do
  cy <- currentYear
  if (y1 > y2 || y1 < 2002 || y2 > cy)
  then error "Wrong year range!must limited between 2002 and now!"
  else
    return . reverse $ filter (not. (\(y,s) -> y == y1 && s <s1 || y == y2 && s >s2 )) ysl where
  ysl = [(y,s) | y <- [y1..y2],s <- [1..4] ]

currentYear :: IO Int -- :: (year,month,day)
currentYear = getCurrentTime >>= return .fromIntegral @_ @Int . fst3 . toGregorian . utctDay

portList = [Nothing, Just 5678,Just 9001,Just 9002,Just 9003,Just 9004,Just 9005,Just 9006]

-- "./module/sinaCodes"
getCYSList :: FilePath -> StartYear -> StartSeason -> EndYear -> EndSeason ->IO [(String,Int,Int)]
getCYSList fp sy ss ey es =  do
  ysList <- getYSList sy ss ey es
  cList <- getStockCodes fp
  let codeList ys =  fmap (oneCYS ys) cList
  return . join $ fmap codeList ysList
  where
    oneCYS ys code = (code,fst ys,snd ys)
    
--gps= onePagePrice mp code year season >>= saveStockPrice
-- onePageRight mp code >>= saveBonusInfo . lefts
gpsDemo = onePagePrice (Just 5678) "000001" 2020 1 >>= saveStockPrice

-- get rightInfo and Save to DataBase,demo
grsDemo = do
  orInfo <- onePageRight (Just 5678) "000002" -- if don't demonad here,it will be calculated twice in below 
  (>>) <$> (saveBonusInfo . lefts ) <*> (saveAllotmentInfo . rights ) $ orInfo
  -- (>>) <$> (saveBonusInfo . lefts =<< ) <*> (saveAllotmentInfo . rights =<< ) $ onePageRight (Just 5678) "000001" --- onePageRight is inside IO, will be calulated twice here,not you want
  
-- MVar [(code,year,season)]
-- one thread get one mp, Maybe PortNumber, take one (code,year,season) from head of MVar List,then putMVar tail of List,so ,these multi-thread cocurrent like this way,when head List is empty,thread putMVar exit to notify main it is ok,when all threads are ok,main ok,out 
