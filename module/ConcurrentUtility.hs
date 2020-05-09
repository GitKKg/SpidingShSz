-- | 

module ConcurrentUtility where

import Control.Concurrent
import Control.Monad
import System.IO
import Text.Printf

import System.Posix.Process.ByteString

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
  syncM <- newEmptyMVar
  mlist <- newMVar [1..103]
  let threadList = ["a","b","c","d","e"]
  traverse (forkIO . threadWork syncM mlist) threadList
  -- traverse (\_ -> takeMVar syncM) threadList -- this make dead loop,why?
  takeMVar syncM
  print "main over"
  where
    threadWork syncM mlist str =  do
      list <-takeMVar mlist
      if null list
        then do
        putStrLn $ str ++  " out for list is empty"
        putMVar syncM str
        else do
        print $ str ++ " get " ++ (show . head) list
        putMVar mlist (tail list)
        threadWork syncM mlist str
