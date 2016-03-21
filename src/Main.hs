{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB

import Blaze.ByteString.Builder.Char.Utf8 (fromChar)
import Control.Monad (unless)
import Control.Monad.Trans (lift)
import Data.Aeson (ToJSON, decode)
import Data.HashMap.Strict (elems, singleton)
import Data.Monoid ((<>))
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Network.Wai.Middleware.Cors (simpleCors)
import System.Directory
import System.FilePath
import System.IO
import System.Process

import Web.Spock.Safe hiding (head)

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

        delete "fs/file" $ do
            path <- param' "path"
            lift $ removeFile path
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

        delete "fs/dir" $ do
            path <- param' "path"
            lift $ removeDirectory path
            text "ok"

        get ("fs/sp-dir" <//> var) $ \name -> do
            dir <- lift $ getSpecialDirectory name
            json [dir]

        post "proc" $ do
            exe <- param' "exe"
            args' <- LB.pack <$> param' "args"
            let (Just args) = decode args'
            (_, Just hout, _, _) <- lift $ createProcess (proc exe args) { std_out = CreatePipe }
            stream $ \send flush ->
                forEachChar hout $ \c -> do
                    send (fromChar c)
                    flush

getSpecialDirectory :: String -> IO FilePath
getSpecialDirectory "Documents" = getUserDocumentsDirectory
getSpecialDirectory "Temporary" = getTemporaryDirectory
getSpecialDirectory "AppData"   = takeDirectory <$> getAppUserDataDirectory "app"

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

forEachChar h f = do
    eof <- hIsEOF h
    unless eof $ hGetChar h >>= f >> forEachChar h f
