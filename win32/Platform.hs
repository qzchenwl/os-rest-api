module Platform where

import System.Win32.Registry

getReg :: String -> String -> IO [(String, String, Int)]
getReg "HKLM" subkey = do
    hkey <- regCreateKey hKEY_LOCAL_MACHINE subkey
    kvs <- regEnumKeyVals hkey
    return $ map (\(k, v, t) -> (k, v, fromIntegral t)) kvs
