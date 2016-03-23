module Platform where

import           Control.Monad
import           System.Directory

getReg :: String -> String -> IO [(String, String, Int)]
getReg _ _ = return []

findExecutable' :: String -> IO (Maybe FilePath)
findExecutable' "chrome" = msum <$> mapM findExecutable ["chrome", "google-chrome", "chromium", "chromium-browser"]
findExecutable' exe = findExecutable exe 
