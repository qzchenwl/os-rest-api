module Platform where

import System.Directory
import System.Win32.Registry

getReg :: String -> String -> IO [(String, String, Int)]
getReg "HKLM" subkey = do
    hkey <- regCreateKey hKEY_LOCAL_MACHINE subkey
    kvs <- regEnumKeyVals hkey
    return $ map (\(k, v, t) -> (k, v, fromIntegral t)) kvs

findExecutable' :: String -> IO (Maybe FilePath)
findExecutable' "chrome" = do
    kvs <- getReg "HKLM" "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\chrome.exe"
    return $ kvs |> find (\(k, _, _) -> k == "") |> \(_, v, _) -> v
findExecutable' exe = findExecutable exe

(|>) :: a -> (a -> b) -> b
x |> f = f x
