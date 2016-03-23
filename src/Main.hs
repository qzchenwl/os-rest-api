{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString                      as B
import qualified Data.ByteString.Lazy.Char8           as LB

import           Blaze.ByteString.Builder             (fromByteString)
import           Control.Monad                        (unless, void, when)
import           Control.Monad.Trans                  (lift)
import           Data.Aeson                           (ToJSON, decode)
import           Data.HashMap.Strict                  (elems)
import           Data.Monoid                          ((<>))
import           Data.Time.Clock                      (UTCTime)
import           GHC.Generics                         (Generic)
import           Network.Wai.Middleware.Cors          (simpleCors)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Process
import           Web.Spock.Safe                       hiding (head)

import           Platform

main :: IO ()
main = startServer 8888

startServer :: Int -> IO ()
startServer port =
    runSpock port $ spockT id $ do
        middleware simpleCors
        middleware logStdout
        middleware logStdoutDev

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

        get "fs/exe" $ do
            path <- lift getExecutablePath
            json [path]

        get ("fs/exe" <//> var) $ \name -> do
            (Just path) <- lift $ findExecutable name
            json [path]

        get "reg/HKLM" $ do
            subkey <- param' "subkey"
            kvs <- lift $ getReg "HKLM" subkey
            json kvs

        post "proc" $ do
            exe <- param' "exe"
            args' <- LB.pack <$> param' "args"
            let (Just args) = decode args'
            (_, Just hout, _, _) <- lift $ createProcess (proc exe args) { std_out = CreatePipe }
            stream $ \send flush ->
                forEachBS hout $ \bs -> do
                    send (fromByteString bs)
                    flush
        post "exit" $ do
            (secret :: String) <- param' "secret"
            when (secret == "3387") (void (lift exitSuccess))
            when (secret == "7833") (void (lift exitFailure))
            return ()

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


data File = File { fileType     :: String
                 , fileName     :: String
                 , lastModified :: UTCTime
                 , absolutePath :: FilePath
                 } deriving (Show, Generic)
instance ToJSON File

forEachBS h f = do
    bs <- B.hGet h 1
    unless (B.null bs) $ f bs >> forEachBS h f
