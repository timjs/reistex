module Output where

resetCode     = "\ESC[0m"

redCode       = "\ESC[31;01m"
greenCode     = "\ESC[32;01m"
yellowCode    = "\ESC[33;01m"
blueCode      = "\ESC[34;01m"
magentaCode   = "\ESC[35;01m"
cyanCode      = "\ESC[36;01m"
whiteCode     = "\ESC[37;01m"

red, green, yellow, blue, magenta, cyan, white :: String -> String
red s     = redCode     ++ s ++ resetCode
green s   = greenCode   ++ s ++ resetCode
yellow s  = yellowCode  ++ s ++ resetCode
blue s    = blueCode    ++ s ++ resetCode
magenta s = magentaCode ++ s ++ resetCode
cyan s    = cyanCode    ++ s ++ resetCode
white s   = whiteCode   ++ s ++ resetCode

putAct, putInf, putErr, putWrn, putInd :: String -> IO ()
putAct s = putStrLn $ green  ">> " ++ s ++ "..."
putInf s = putStrLn $ white  ":: " ++ s
putErr s = putStrLn $ red    "!! " ++ s
putWrn s = putStrLn $ yellow "** " ++ s
putInd s = putStrLn $        "   " ++ s

{-
(<>) :: (Show a, Show b) => a -> b -> String
{-# INLINE (<>) #-}
{-# SPECIALIZE (<>) :: (Show a) => a -> String -> String #-}
{-# SPECIALIZE (<>) :: (Show b) => String -> b -> String #-}
{-# SPECIALIZE (<>) :: String -> String -> String #-}
a <> b = show a ++ show b
-}
