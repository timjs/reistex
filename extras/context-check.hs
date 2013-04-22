module Main where

import Data.List
import Data.Char

import System.Environment

import Output

type Stack a = [a]
type Error   = String
type Line    = Int

count :: (Eq a) => a -> [a] -> Int
count _ []     = 0
count x (y:ys)
  | x == y     = 1 + count x ys
  | otherwise  = count x ys

isOpening :: String -> Maybe (String, String)
isOpening s
  | Just r <- stripPrefix "{" s      = Just ("}", r)
  | Just r <- stripPrefix "[" s      = Just ("]", r)
  | Just r <- stripPrefix "(" s      = Just (")", r)
  | Just r <- stripPrefix "\\start" s
  , (n,r)  <- span isLetter r        = Just ("\\stop" ++ n, r)
  | otherwise                        = Nothing

isClosing :: String -> Maybe (String, String)
isClosing s
  | Just r <- stripPrefix "}" s      = Just ("}", r)
  | Just r <- stripPrefix "]" s      = Just ("]", r)
  | Just r <- stripPrefix ")" s      = Just (")", r)
  | Just r <- stripPrefix "\\stop" s
  , (n,r)  <- span isLetter r        = Just ("\\stop" ++ n, r)
  | otherwise                        = Nothing

isNewLine :: String -> Maybe String
isNewLine s = stripPrefix "\n" s

isComment :: String -> Maybe String
isComment s
  | Just r <- stripPrefix "%" s
  , r      <- dropWhile (/= '\n') s = Just r
  | otherwise                       = Nothing

{- Het type van deze functie vraagt eigenlijk om een Writer Monad,
 - maar omdat dit maar een script is, gaan we het ons niet te ingewikkeld maken...
 -}
balanced :: Line -> Stack (Line,String) -> String -> (Bool,Error) --Writer Error Bool
balanced _ []         ""      = (True, "")
balanced l ((k,o):_)  ""      = (False, "Line " ++ show l ++ ":\n   " ++ "Unexpected end of file, expected '" ++ o ++ "'\n   " ++ "(to match with line " ++ show k ++ ")")
balanced l os s
  | Just r     <- isNewLine s = balanced (l+1) os r
  | Just r     <- isComment s = balanced l os r
  | Just (o,r) <- isOpening s = balanced l ((l,o):os) r
  | Just (c,r) <- isClosing s = case os of
    []                       -> (False, "Line " ++ show l ++ ":\n   " ++ "Unexpected '" ++ c ++ "', closed withoud opening")
    (k,o):os    | c == o     -> balanced l os r
                | otherwise  -> (False, "Line " ++ show l ++ ":\n   " ++ "Unexpected '" ++ c ++ "', expected '" ++ o ++ "'\n   " ++ "(to match with line " ++ show k ++ ")")
  | otherwise                 = balanced l os $ tail s

check :: String -> (Bool,Error)
check = balanced 1 []

run :: FilePath -> IO Bool
run f = do
  putAct f
  s <- readFile f
  case check s of
    (True,_)  -> return True
    (False,e) -> putErr e >> return False

main = do
  as <- getArgs
  rs <- mapM run as
  let n = count False rs
  case n of
    0 -> putInf "Everything seems to be all right!"
    1 -> putInf "Oops, found 1 error!"
    n -> putInf $ "Oh help, found " ++ show n ++ " errors!"

