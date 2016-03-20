{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Web.Spock.Safe
import System.IO
import Control.Concurrent
import Blaze.ByteString.Builder.Char.Utf8

main :: IO ()
main = do
    runSpock 8080 $ spockT id $ do
        get root $ do
            setHeader "Access-Control-Allow-Origin" "*"
            stream $ \send flush -> do
                h <- openFile "stack.yaml" ReadMode
                slowRead h $ \c -> do
                    send (fromChar c)
                    flush
                    threadDelay 500000

slowRead h f = do
    eof <- hIsEOF h
    if eof
       then return ()
       else hGetChar h >>= f >> slowRead h f
