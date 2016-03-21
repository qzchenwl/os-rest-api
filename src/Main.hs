{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Web.Spock.Safe
import System.IO
import Control.Monad.Trans (lift)
import Control.Concurrent
import Blaze.ByteString.Builder.Char.Utf8
import Network.Wai.Middleware.Cors
import System.Directory
import System.FilePath

main :: IO ()
main = do
    runSpock 8080 $ spockT id $ do
        middleware simpleCors
        get "fs/file" $ do
            f <- param' "file"
            file f

        get "fs/dir" $ do
            dir <- param' "dir"
            text dir
        get ("fs/sp-dir" <//> var) $ \name -> do
            dir <- lift $ getSpecialDirectory name
            json [dir]

        get root $ do
            stream $ \send flush -> do
                h <- openFile "stack.yaml" ReadMode
                slowRead h $ \c -> do
                    send (fromChar c)
                    flush
                    threadDelay 500000

getSpecialDirectory :: String -> IO FilePath
getSpecialDirectory "Documents" = getUserDocumentsDirectory
getSpecialDirectory "Temporary" = getTemporaryDirectory
getSpecialDirectory "AppData"   = getAppUserDataDirectory "app" >>= return .takeDirectory

slowRead h f = do
    eof <- hIsEOF h
    if eof
       then return ()
       else hGetChar h >>= f >> slowRead h f
