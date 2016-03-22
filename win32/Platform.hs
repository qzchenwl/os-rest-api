module Platform where

getReg :: String -> String -> IO [(String, String, Int)]
getReg "HKLM" subkey = do
    hkey <- regCreateKey hKEY_LOCAL_MACHINE subkey
    kvs <- regEnumKeyVals hkey
    return $ map (\(k, v, type) -> (k, v, fromIntegral type)) kvs

