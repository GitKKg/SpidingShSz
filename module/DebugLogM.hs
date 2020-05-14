-- | 
{-# LANGUAGE OverloadedStrings ,TypeApplications #-}
module DebugLogM where

import Debug.Trace
import System.IO.Unsafe
import Control.Concurrent
import System.Directory
import System.IO
import Control.Monad

--import Control.Exception
--import Formatting
--import Formatting.Clock


logOutM str = (>>) <$> logM  <*> traceM $ str

-- trace :: String -> a -> a
-- trace string expr = unsafePerformIO $ do
--     traceIO string
--     return expr

-- traceM :: Applicative f => String -> f ()
-- traceM string = trace string $ pure ()

logM ::  Applicative f =>  String -> f()
logM  str = unsafePerformIO $ do
  createDirectoryIfMissing True "./log"
  id <- myThreadId
  let log = "./log/" ++ ((!! 1) . words . show) id ++ "log.txt"
  fileExist <- doesFileExist $ log
  when (not fileExist) $ openFile log WriteMode >>= hClose
  appendFile log str
  return $ pure ()
