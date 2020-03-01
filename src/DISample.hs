{-# LANGUAGE FlexibleInstances #-}

module DISample where

import           Prelude                 hiding ( log )
import           Control.Monad.IO.Class
import           Control.Monad.Identity
import           System.IO
import           Control.Monad.State

-- Any function that can turn a string into an action is considered a Logger.
type Logger m = String -> m ()

-- Logger that does nothing, for testing.
noLogger :: Monad m => Logger m
noLogger _ = return ()

-- Logger that prints to STDERR.
stderrLogger :: MonadIO m => Logger m
stderrLogger s = liftIO $ hPutStrLn stderr s

-- Logger that appends messages to a given file.
fileLogger :: MonadIO m => FilePath -> Logger m
fileLogger filePath s = liftIO logToFile
    where logToFile = withFile filePath AppendMode $ flip hPutStrLn s

-- Programs have to provide a way to get the logger to use.
-- 多分重要なのは、上のロガーたちをクラスのインスタンスにする訳ではないということ。
-- あるモナドをこの型クラスのインスタンスにすれば、そのモナドの中で使えるロガーを作成できる。
class Monad m => MonadLogger m where
    getLogger :: m (Logger m)

-- Logs a given string using the logger obtained from the environmet.
log :: MonadLogger m => String -> m ()
log s = join $ getLogger <*> return s

-- Example function that we want to run in different contexts,
-- like skip logging during testing.
printFile :: (MonadIO m, MonadLogger m) => FilePath -> m ()
printFile fp = do
    log ("Printing file: " ++ fp)
    liftIO $ readFile fp >>= putStr
    log "Done Printing."

{-
   For real program
-}

-- Let's say this is the real program: it keeps log file name using StateT.
type RealProgram = StateT String IO

-- To get the logger, build the right fileLogger.
instance MonadLogger RealProgram where
    getLogger = fileLogger <$> get

-- And this is how you run printFile "for real".
realMain :: IO ()
realMain = evalStateT (printFile "data/file-to-print.txt") "logs/log.txt"

{-
    For test
-}

-- This is a fake program for testing: it will not do any logging.
type FakeProgramForTesting = IO

-- Use noLogger
instance MonadLogger FakeProgramForTesting where
    getLogger = return noLogger

-- This program doesn't do any logging, but still does IO.
fakeMain :: IO ()
fakeMain = printFile "data/file-to-print.txt"
