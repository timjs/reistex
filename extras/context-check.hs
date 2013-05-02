module Main where

import Data.List
import Data.Char
import Data.Maybe

import Control.Monad

import System.Environment
import System.Exit

import Output

{- Allereerst definieeren we een paar type-synoniemen.
 - We gebruiken een eenvoudige lijst als stapel (Stack),
 - een foutmelding is een string en een regelnummer is een geheel getal,
 - stapelsymbolen zijn ook strings.
 -}
type Stack a = [a]
type Error   = String
type Line    = Int

data Symbol  = Brace
             | Bracket
             | Paren
             | Math
             | Delim
             | Stop Name
             | End  Name
             deriving (Eq, Ord)
type Name    = String

instance Show Symbol where
  show Brace    = "}"
  show Bracket  = "]"
  show Paren    = ")"
  show Math     = "$"
  show Delim    = "\\right"
  show (Stop n) = "\\stop" ++ n
  show (End n)  = "\\end{" ++ n ++ "}"

{-| O(n) Bereken hoe vaak @x@ in de lijst voorkomt. -}
count :: (Eq a) => a -> [a] -> Int
count _ []     = 0
count x (y:ys)
  | x == y     = 1 + count x ys
  | otherwise  = count x ys
-- count x = length . filter (== x) -- maar is O(n+m)

{-| Controleer of de string begint met een openingshaakje of een start-commando.
 -  Als dat zo is, strippen we het deze tekst van de sting en geven we `gewoon' een paar terug
 -  met daarin het sluithaakje of stop-commando en de rest van de string.
 -  Als we niets vinden, geven we `niets' terug!
 -}
isOpening :: Bool -> String -> Maybe (Symbol, Bool, String)
isOpening m s
  | Just r <- stripPrefix "$" s      = if m then Nothing else Just (Math, True, r)
  | Just r <- stripPrefix "{" s      = Just (Brace, m, r)
  | Just r <- stripPrefix "[" s      = Just (Bracket, m, r)
  | Just r <- stripPrefix "(" s      = Just (Paren, m, r)
  | Just r <- stripPrefix "\\left" s
  , Just r <- isDelimiter r          = Just (Delim, m, r)
  | Just r <- stripPrefix "\\start" s
  , (n,r)  <- span isLetter r        = Just (Stop n, m, r)
  | Just r <- stripPrefix "\\begin{" s
  , (n,r)  <- span isLetter r
  , Just r <- stripPrefix "}" r      = Just (End n, m, r)
  | otherwise                        = Nothing

{-| Analoog aan @isOpening@, maar dan voor sluithaakjes en stop-commando's.
 -}
isClosing :: Bool -> String -> Maybe (Symbol, Bool, String)
isClosing m s
  | Just r <- stripPrefix "$" s      = if m then Just (Math, False, r) else Nothing
  | Just r <- stripPrefix "}" s      = Just (Brace, m, r)
  | Just r <- stripPrefix "]" s      = Just (Bracket, m, r)
  | Just r <- stripPrefix ")" s      = Just (Paren, m, r)
  | Just r <- stripPrefix "\\right" s
  , Just r <- isDelimiter r          = Just (Delim, m, r)
  | Just r <- stripPrefix "\\stop" s
  , (n,r)  <- span isLetter r        = Just (Stop n, m, r)
  | Just r <- stripPrefix "\\end{" s
  , (n,r)  <- span isLetter r
  , Just r <- stripPrefix "}" r      = Just (End n, m, r)
  | otherwise                        = Nothing

{-| Controleer of de string begint met een nieuwe regel.
 -  Strip deze en geef de rest van de string terug.
 -}
isNewLine :: String -> Maybe String
isNewLine s = stripPrefix "\n" s

{-| Controleer of de string begint met een procent teken.
 -  In dat geval gooien we alle tekens weg tot het einde van de regel,
 -  en geven de rest van de string terug.
 -}
isComment :: String -> Maybe String
isComment s
  | Just r <- stripPrefix "%" s      = Just $ dropWhile (/= '\n') r
  | otherwise                        = Nothing

isEscaped :: String -> Maybe String
isEscaped s = listToMaybe $ mapMaybe (flip stripPrefix $ s)
  [ "\\%"
  , "\\$"
  ]

isDelimiter :: String -> Maybe String
isDelimiter s = listToMaybe $ mapMaybe (flip stripPrefix $ s)
  [ "."
  , "("
  , ")"
  , "\\{"
  , "\\}"
  , "["
  , "]"
  , "<"
  , ">"
  , "|"
  , "\\|"
  , "/"
  , "\\lgroup"
  , "\\rgroup"
  , "\\lbrace"
  , "\\rbrace"
  , "\\langle"
  , "\\rangle"
  , "\\vert"
  , "\\lvert"
  , "\\rvert"
  , "\\Vert"
  , "\\lVert"
  , "\\rVert"
  , "\\backslash"
  , "\\lfloor"
  , "\\rfloor"
  , "\\lceil"
  , "\\rceil"
  , "\\uparrow"
  , "\\Uparrow"
  , "\\downarrow"
  , "\\Downarrow"
  , "\\updownarrow"
  , "\\Updownarrow"
  , "\\llcorner"
  , "\\lrcorner"
  , "\\ulcorner"
  , "\\urconrner"
  , "\\lmoustache"
  , "\\rmoustache" ]

{-| Recursief algoritme om te controleren of de string gebalanceerd is
 -  wat betreft haakjes en start/stop-paren.
 -
 -  Een korte omschrijving per regel:
 -  * Een lege string met een lege stapel is natuurlijk gebalanceerd.
 -  * Een lege string met een niet lege stapel gaat helemaal mis.
 -  * In andere gevallen:
 -    * We houden het regelnummer bij en hoogen het op als we een regeleinde tegenkomen.
 -    * Commentaar gooien we weg.
 -    * Wanneer we een openingshaakje tegenkomen zetten we het huidige regelnummer en
 -      het sluithaakje op de stapel.
 -    * Wanneer we een sluithaakje tegenkomen controleren we of dit op de stapel stond.
 -    * Als de string niet begint met een haakje of start/stop-paar,
 -      dan gaan we door met de rest van de tekst.
 -
 -  Het type van deze functie vraagt eigenlijk om een Writer Monad,
 -  maar omdat dit maar een script is, gaan we het ons niet te ingewikkeld maken...
 -}
balanced :: Line -> Stack (Line,Symbol) -> Bool -> String -> (Bool,Error) --Writer Error Bool
balanced _ []        False  ""    = (True, "")
balanced l ((k,o):_) _      ""    = (False, "Line " ++ show l ++ ":\n   " ++ "Unexpected end of file, expected '" ++ show o ++ "'\n   " ++ "(to match with line " ++ show k ++ ")")
balanced l os m s
  | Just r       <- isNewLine s   = balanced (l+1) os m r
  | Just r       <- isEscaped s   = balanced l os m r
  | Just r       <- isComment s   = balanced l os m r
  | Just (o,m,r) <- isOpening m s = balanced l ((l,o):os) m r
  | Just (c,m,r) <- isClosing m s = case os of
    []                           -> (False, "Line " ++ show l ++ ":\n   " ++ "Unexpected '" ++ show c ++ "', closed withoud opening")
    (k,o) : os  | c == o         -> balanced l os m r
                | otherwise      -> (False, "Line " ++ show l ++ ":\n   " ++ "Unexpected '" ++ show c ++ "', expected '" ++ show o ++ "'\n   " ++ "(to match with line " ++ show k ++ ")")
  | otherwise                     = balanced l os m $ tail s

{-| Controleer of de string gebalanceerd.
 -  We beginnen bij regel 1 met een lege stapel.
 -}
check :: String -> (Bool,Error)
check = balanced 1 [] False

{-| Lees de tekst in uit een bestand, controleer of deze gebalanceerd is
 -  en geef het resultaat door.
 -}
run :: FilePath -> IO Bool
run f = do
  putAct f
  s <- readFile f
  case check s of
    (True,_)  -> return True
    (False,e) -> putErr e >> return False

{-| Hoofdfunctie waarbij we de commandoregel argumenten inlezen
 -  en controleren of die er uberhaupt wel zijn.
 -  Vervolgens passen we @run@ toe op elk argumente
 -  en geven nog een conclusie of het aantal fouten.
 -}
main = do
  as <- getArgs
  when (null as) $ do
    putWrn "Without arguments I can't do anything!"
    exitFailure

  rs <- mapM run as
  let n = count False rs
  case n of
    0 -> putInf "Everything seems to be all right!"
    1 -> putInf "Oops, found 1 error!"
    n -> putInf $ "Oh help, found " ++ show n ++ " errors!"
  exitSuccess

-- vim: nowrap
