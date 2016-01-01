module Odin.Control.File where

import Control.Varying
import System.IO
import Control.Monad.IO.Class

fileLines :: MonadIO m => FilePath -> Var m a [String]
fileLines fp = Var $ \_ -> do
    h <- liftIO $ openFile fp ReadMode
    handleLines h
        where handleLines h = do lns <- liftIO $ readTillEnd h
                                 return (lns, Var $ const $ handleLines h)

readTillEnd :: Handle -> IO [String]
readTillEnd h = do
    eof <- hIsEOF h
    if eof
    then return []
    else do ln  <- hGetLine h
            lns <- readTillEnd h
            return $ ln:lns
