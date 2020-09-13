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

import ConcurrentUtility

import System.Directory
import System.IO
import System.Environment

import Debug.Trace
import Control.Monad.Trans

import System.Clock

import Control.Monad.State
import Control.Monad.State.Lazy

import System.IO.Unsafe

import Data.Maybe

import Control.Exception

import DebugLogM

-- demo for State
type NameGen = State Int

newName :: State Int String -- state s is Int,and eval output a is String, State s a
newName = state $ \i -> ("x" ++ show i, i + 1)

-- state can only be passed in Monad way,so must be >> or >>= like series
x8Name = evalState (newName >> newName >> newName) 6 -- newName is calculated 3 times,and intial state is 6, so state is 6+2 =8, and evalue output a is "x8"
-- "x8"

nameT :: StateT Int IO String
nameT = StateT $ \i -> return ("x" ++ show i, i + 1)

x8NameT = runStateT  (nameT >> nameT >> nameT) 6

tickT :: StateT Int IO Int
tickT = do
  n <- get
  traceShowM n
  put (n+1)
  return n
plusT :: Int -> Int -> IO Int
plusT n x = execStateT (sequence $ replicate n tickT) x

tick :: State Int Int
tick = do n <- get
          put (n+1)
          return n -- State s a, n is a
          
plus :: Int -> Int -> Int
plus n x = execState (sequence $ replicate n tick) x -- here,tick as State ,is evaluated n times in sequence ,so state is already +n

-- a clear IO in StateT demo
--  use StateT to embed IO inside, for Identity of State get no MonadIO or MonadTrans instance
-- without  IO could not introduce liftIO and lift,IO get MonadIO and MonadTrans instance
secS :: StateT Int IO Int
secS = do
  lastSec <- get
  lift  $  traceM. (++ " second") . ("last time is " ++).show $ lastSec
  -- let end = getTime Monotonic
  newSec <-lift $ do
    end <- getTime Monotonic
    let newSec = round . (/ 10 ^9 ) . fromInteger . toNanoSecs $ end
    traceShowM newSec
    return newSec
  liftIO $ traceM "wait for 2 seconds" >> threadDelay 2000000
  put newSec
  --return lastSec
  secS -- infite loop
-- execStateT secS 0 
-- last time is 0 second
-- 25554
-- wait for 2 seconds
-- last time is 25556 second
-- 25556
-- wait for 2 seconds
-- 25558
-- infite loop ...

  
timing1s =
  do start <- getTime Monotonic
     --evaluate (sum [1 .. 1000000])
     threadDelay $ 10^6 *2 -- 2s
     end <- getTime Monotonic
     --fprint (timeSpecs % "\n") start end
     return $  fromIntegral (toNanoSecs end - toNanoSecs start) / (10 ^9) -- in  second

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
  if y1 > y2 || y1 < 2002 || y2 > cy
  then error "Wrong year range!must limited between 2002 and now!"
  else
    return . reverse $ filter (not. (\(y,s) -> y == y1 && s <s1 || y == y2 && s >s2 )) ysl where
  ysl = [(y,s) | y <- [y1..y2],s <- [1..4] ]

currentYear :: IO Int -- :: (year,month,day)
currentYear = getCurrentTime >>= return .fromIntegral @_ @Int . fst3 . toGregorian . utctDay

portList = [Nothing, Just 5678,Just 9000,Just 9001,Just 9002,Just 9003,Just 9004,Just 9005,Just 9006]
--portList = [Just 5678]
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
-- gpsDemo = snd <$> (onePagePrice (Just 5678) "000001" 2020 1) >>= saveStockPrice "0"
-- new demo to be compatible with early exit feature for proxy threads
-- set error port could test mzero exception
gpsDemo = do
  mlist <- newMVar [("000001" , 2020, 1)]
  ee <- try @SomeException $ snd <$> (onePagePrice mlist (Just 10808) 10 "000001" 2020 1 False) >>= saveStockPrice "0"
  case ee of
    Left e -> do
      logOutM $ "exception when onePagePrice! is \n" ++ show e ++ "\n it seems proxy thread demand out!\n"
    Right ok -> return ()

-- get rightInfo and Save to DataBase,demo
grsDemo = do
  mlist <- newMVar ["000001"]
  (endSec, orInfo) <- onePageRight mlist (Just 5678) 10  "000002" -- if don't demonad here,it will be calculated twice in below 
  (>>) <$> (saveBonusInfo "0" . lefts ) <*> (saveAllotmentInfo "0" . rights ) $ orInfo
  -- (>>) <$> (saveBonusInfo . lefts =<< ) <*> (saveAllotmentInfo . rights =<< ) $ onePageRight (Just 5678) "000001" --- onePageRight is inside IO, will be calulated twice here,not you want

-- MVar [(code,year,season)]
-- one thread get one mp, Maybe PortNumber, take one (code,year,season) from head of MVar List,then putMVar tail of List,so ,these multi-thread cocurrent like this way,when head List is empty,thread putMVar exit to notify main it is ok,when all threads are ok,main ok,out 
--wmain :: StartYear -> StartSeason -> EndYear -> EndSeason -> FilePath -> IO()
--logOut :: String -> IO ()
logOut log = (>>) <$> liftIO . (appendFile log) <*> traceM

-- how to set args for main in GHCI
-- :set args 2018 4 2019  1 "./module/sinaCodes"
--threadWorkT :: MVar Char -> MVar [(String , Int,Int)] -> Maybe PortNumber -> StateT Int IO ()
main = do
  args <- getArgs
  let sy = read @StartYear $ args!!0
  traceM $ show sy
  let ss = read @StartSeason $ args!!1
  traceM $ show ss
  let ey = read @EndYear $ args!!2
  traceM $ show ey
  let es = read @EndSeason $ args!!3
  traceM $ show es
  let fp = args!!4 -- "./module/sinaCodes"--read @String $ args!!4
  traceM $ fp
  cysList <- getCYSList fp sy ss ey es
  --traceShowM cysList
  (>>=) <$> doesDirectoryExist <*>  ((flip when) . removeDirectoryRecursive)  $ "./log"
  createDirectoryIfMissing True "./log"

  -- Price Spiding
  syncP <- newEmptyMVar
  mlistP <- newMVar cysList

  -- just one syncD for sync with main is not suitble,MVar is not count,is get one to take one
  --syncD <- newEmptyMVar -- for main wait for Direct link thread
  syncDP <- newEmptyMVar -- for main wait for Direct link thread of Pricing
  syncDab <- newEmptyMVar -- for main wait for Direct link thread of AB
  let threadList = portList
  mapM (forkIO . threadWorkP 0 syncP syncDP mlistP  threadList False) threadList
  --mapM (forkIO . evalStateT .threadWorkT  syncM mlist ) threadList

  -- Bonus and Allotment Spiding
  codeList <- getStockCodes fp
  syncAB <- newEmptyMVar
  mlistAB <- newMVar codeList

  mapM (forkIO . threadWorkAB 0 syncAB syncDab mlistAB threadList False) threadList

  -- mapM_ (\_ -> takeMVar syncAB) threadList
  takeMVar syncDab

  print "Bonus and Allotment right info spiding are over! \n"
  
  -- mapM (\_ -> takeMVar syncP) threadList
  takeMVar syncDP
  
  print "Price spiding over,main out\n"
  where
    -- just messy to use StateT here,it must pass lastSec in to onePage here,and output [stock] out from onePage then,this make StateT lost sense,since passing parameters is not avoidable,just use non-State way
    threadWorkP lastSec syncM syncD mlist threadList allOut mayPort  = do
      id <- myThreadId
      let log = "./log/" ++ ((!! 1) . words . show) id ++ "log.txt"
      fileExist <- doesFileExist $ log
      when (not fileExist) $ openFile log WriteMode >>= hClose
      list <-takeMVar mlist
      id <- myThreadId
      if null list
        then do
        logOut log $ show id ++  " out for list is empty \n"
        putMVar mlist list -- if not put back,other thread will block
        case (isNothing mayPort) of
          True -> do -- Direct link thread
            case (not allOut) of
              True -> do -- proxy threads are not out
                -- should not count yourself,for proxy may out before you,and putMVar before you,so block your putMVar
                --putMVar syncM 'x' -- include count of yourself
                logOut log $ show id ++ " as Direclink, wait for all proxy threads out !\n"
                -- wait for all proxy threads out,except direct link
                mapM_ (\_ -> takeMVar syncM) (tail threadList)
                logOut log $ show id ++ " as Direclink, known all proxy threads out !\n"
                threadWorkP lastSec syncM syncD mlist threadList True mayPort
              otherwise -> do
                logOut log $ show id ++ " as Directlink, notify main to out\n"
                putMVar syncD 'x' -- Direclink thread out
          otherwise -> do
            logOut log $ show id ++ " as proxy thread , is normally out ,list empty\n"
            putMVar syncM 'x' -- proxy thread out
        
        else do
        let (code,year,season) = head list
        logOut log $ show id ++ " get " ++ (show . head) list ++ "\n"
        putMVar mlist (tail list)
        startT <- getTime Monotonic 
        let startSec = round . (/ (10^9) ) . fromInteger . toNanoSecs $ startT -- in sec
        logOut log $ "start time is " ++ show startSec ++ "s\n"
        when (startSec - lastSec < 6) $ do
          let waitSec = 6-(startSec- lastSec)
          logOut log $ "less than 6s ,wait for " ++ show waitSec ++ "s\n"
          threadDelay $ waitSec * (10^6)
        -- evalState
        ePInfo <- try @SomeException $ onePagePrice mlist mayPort (Prelude.length threadList) code year season False
        case ePInfo of
          Left e -> do
            logOutM $ "exception when onePagePrice! is \n" ++ show e ++ "\n it seems proxy thread demand out!\n"
            putMVar syncM 'x'
          Right info -> do
            let (endSec ,stockL) = info
            --endT <- getTime Monotonic
            -- let endSec = round . (/ (10^9) ) . fromInteger . toNanoSecs $ endT
            logOut log $ "end time is " ++ show endSec ++ "s\n"
            -- some stock just exit market or not go in public in that time range,so stockL maybe null
            -- for example, 000022,when 2019 exit market
            when (not . null $ stockL) $ saveStockPrice (show id) stockL
            --end <- getTime Monotonic
            --let duration = fromIntegral (toNanoSecs end - toNanoSecs start) / 10 ^9
            --when (duration < 6) $ threadDelay $ round (6-duration) * 10 ^6 -- wait for at least 6 second to avoid ban
            threadWorkP endSec syncM syncD mlist threadList allOut mayPort
        
    threadWorkAB lastSec syncM syncD mlist threadList allOut mayPort = do -- evalStateT
      id <- myThreadId
      let log = "./log/" ++ ((!! 1) . words . show) id ++ "log.txt"
      fileExist <- doesFileExist $ log
      when (not fileExist) $ openFile log WriteMode >>= hClose
      list <-takeMVar mlist
      id <- myThreadId
      if null list
        then do
        logOut log $ show id ++  " out for list is empty \n"
        putMVar mlist list -- if not put back,other thread will block
        case (isNothing mayPort) of
          True -> do -- Direct link thread
            case (not allOut) of
              True -> do -- proxy threads are not out
                -- should not count yourself,for proxy may out before you,and putMVar before you,so block your putMVar
                --putMVar syncM 'x' -- include count of yourself
                logOut log $ show id ++ " as Direclink, wait for all proxy threads out!\n"
                -- wait for all proxy threads out,except direct link
                mapM_ (\_ -> takeMVar syncM) (tail threadList)
                logOut log $ show id ++ " as Direclink, known all proxy threads out!\n"
                threadWorkAB lastSec syncM syncD mlist threadList True mayPort
              otherwise -> do
                logOut log $ show id ++ " as Directlink, notify main to out"
                putMVar syncD 'x'
          otherwise -> do
            logOut log $ show id ++ " as proxy thread , is normally out ,list empty\n"
            putMVar syncM 'x' -- proxy thread out
  
        else do
        let code = head list
        logOut log $ show id ++ " get " ++ (show . head) list ++ "\n"
        putMVar mlist (tail list)
        startT <- getTime Monotonic 
        let startSec = round . (/ (10^9) ) . fromInteger . toNanoSecs $ startT -- in sec
        logOut log $ "start time is " ++ show startSec ++ "s\n"
        when (startSec - lastSec < 6) $ do
          let waitSec = 6-(startSec- lastSec)
          logOut log $ "less than 6s ,wait for " ++ show waitSec ++ "s\n"
          threadDelay $ waitSec * (10^6)

        eRinfo <- try @SomeException $ onePageRight mlist mayPort (Prelude.length threadList) code
        case eRinfo of
          Left e -> do
            logOutM $ "exception when onePagePrice! is \n" ++ show e ++ "\n it seems proxy thread demand out!\n"
            putMVar syncM 'x'
          Right info -> do
            let (endSec,stockL) = info
            logOut log $ "end time is " ++ show endSec ++ "s\n"
            -- some stock just exit market or not go in public in that time range,so stockL maybe null
            -- for example, 000022,when 2019 exit market
            -- when (not . null $ stockL) $ saveStockPrice (show id) stockL
            when (not . null $ stockL) $ (>>) <$> (saveBonusInfo (show id) . lefts ) <*> (saveAllotmentInfo (show id) . rights ) $ stockL
            --end <- getTime Monotonic
            --let duration = fromIntegral (toNanoSecs end - toNanoSecs start) / 10 ^9
            --when (duration < 6) $ threadDelay $ round (6-duration) * 10 ^6 -- wait for at least 6 second to avoid ban
            threadWorkAB lastSec syncM syncD mlist threadList allOut mayPort
