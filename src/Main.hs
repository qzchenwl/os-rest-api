{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Monoid ((<>))
import Web.Spock.Safe hiding (head)
import System.IO
import Control.Monad.Trans
import Control.Concurrent
import Blaze.ByteString.Builder.Char.Utf8
import Network.Wai.Middleware.Cors
import System.Directory
import System.FilePath
import qualified Data.ByteString as B
import Data.HashMap.Strict (elems, singleton)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime)

main :: IO ()
main = do
    runSpock 8080 $ spockT id $ do
        middleware simpleCors

        get "fs/file" $ do
            path <- param' "path"
            content <- lift $ B.readFile path
            bytes content

        post "fs/file" $ do
            path <- param' "path"
            tempLocation <- uf_tempLocation . head . elems <$> files
            lift $ copyFile tempLocation path
            text "ok"

        get "fs/dir" $ do
            path <- param' "path"
            contents <- lift $ getDirectoryContents path
            files <- lift $ mapM (getFileStat path) (filter (\fn -> fn /= "." && fn /= "..") contents)
            json files

        post "fs/dir" $ do
            path <- param' "path"
            lift $ createDirectoryIfMissing True path
            text "ok"

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

-- FIXME handle exception
getFileStat dir fn = do
    isDir <- doesDirectoryExist (dir </> fn)
    modified <- getModificationTime (dir </> fn)
    let ft = if isDir then "directory" else "file"
     in return (File ft fn modified (dir </> fn))


data File = File { fileType :: String
                 , fileName :: String
                 , lastModified :: UTCTime
                 , absolutePath :: FilePath
                 } deriving (Show, Generic)
instance ToJSON File

slowRead h f = do
    eof <- hIsEOF h
    if eof
       then return ()
       else hGetChar h >>= f >> slowRead h f
