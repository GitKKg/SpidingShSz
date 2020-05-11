-- | 

module ConcurrentUtility where

import Control.Concurrent
import Control.Monad
import System.IO
import Text.Printf

import System.Posix.Process.ByteString

import System.Directory
import System.IO

test = do
  hSetBuffering stdout NoBuffering
  forkIO $ (replicateM_ 100 ) (putChar 'A')
  replicateM_ 100 (putChar 'B')

reminder =
  forever $ do
  s <- getLine
  forkIO $ setReminder s

setReminder :: String -> IO ()
setReminder s = do
  coreNum <- getNumCapabilities
  printf "numCapbility is %d \n"  coreNum
  setNumCapabilities 2
  print "now set coreNum as 2 \n"
  let t = read s :: Int
  printf "\nok, I'll remind you in %d second \n" t
  threadDelay (10^6 * t)
  printf "%d seconds is up! BING!\BEL\n" t
  getProcessID >>= print

testMvList = do
  -- a run before b in ... <$> a <*> b $ ... as shown below
  (>>=) <$> doesDirectoryExist <*>  ((flip when) . removeDirectoryRecursive)  $ "./log"
  createDirectoryIfMissing True "./log"
  syncM <- newEmptyMVar
  mlist <- newMVar [1..103]
  let threadList = ["a","b","c","d","e"]
  mapM (forkIO . threadWork syncM mlist) threadList
  mapM (\_ -> takeMVar syncM) threadList -- this make dead loop,why?,because below thread not putMVar mlist back
  --takeMVar syncM
  print "main over"
  where
    threadWork syncM mlist str =  do
      id <- myThreadId
      let log = "./log/" ++ ((!! 1) . words . show) id ++ "log.txt"
      fileExist <- doesFileExist $ log
      when (not fileExist) $ openFile log WriteMode >>= hClose
      list <-takeMVar mlist
      if null list
      then do
        (>>) <$> appendFile log <*> putStrLn $ str ++  " out for list is empty \n"
        putMVar mlist list -- if not put back,other thread will block
        putMVar syncM str
      else do
        (>>) <$> appendFile log <*> putStrLn $ str ++ " get " ++ (show . head) list ++ "\n"
        myThreadId >>= print
        putMVar mlist (tail list)
        threadWork syncM mlist str
