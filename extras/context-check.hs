module Main where

import Data.List
import Data.Char
import Data.Maybe

import Control.Monad

import System.Environment
import System.Exit

import Output

-- Allereerst definiëren we een paar type-synoniemen. We gebruiken een
-- eenvoudige lijst als stapel (Stack), een foutmelding is een string en een
-- regelnummer is een geheel getal.
type Stack a = [a]
type Error   = String
type Line    = Int

-- | Een symbool is een abstract data type. Deze kunnen we goedkoper op de
-- stapel zetten dan strings. Een kleine optimalisatie dus. We leiden een
-- instantie voor @Eq@ en @Ord@ af en maken er zelf eentje voor @Show@ aan.
data Symbol  = Brace
             | Bracket
             | Paren
             | Math
             | Delim
             | Block Name
             | Env   Name
             deriving (Eq, Ord)
type Name    = String

open :: Symbol -> String
open s = case s of
  Brace   -> "{"
  Bracket -> "["
  Paren   -> "("
  Math    -> "$"
  Delim   -> "\\left"
  Block n  -> "\\start" ++ n
  Env n   -> "\\begin{" ++ n ++ "}"

close :: Symbol -> String
close s = case s of
  Brace   -> "}"
  Bracket -> "]"
  Paren   -> ")"
  Math    -> "$"
  Delim   -> "\\right"
  Block n  -> "\\stop" ++ n
  Env n   -> "\\end{" ++ n ++ "}"

-- | /O(n)/ Bereken hoe vaak @x@ in de lijst voorkomt.
count :: (Eq a) => a -> [a] -> Int
count _ []     = 0
count x (y:ys)
  | x == y     = 1 + count x ys
  | otherwise  = count x ys
-- count x = length . filter (== x) -- maar is O(n+m)

-- | Controleer of de string begint met een openingshaakje, een start- of
-- begin-commando of met wiskunde. Als dat zo is, strippen we het deze tekst van
-- de string en geven we `gewoon' een paar terug met daarin het sluithaakje of
-- stop-commando en de rest van de string. Als we niets vinden, geven we `niets'
-- terug!
--
-- Het dollar teken is een vreemde eend in de bijt. We kunnen namelijk niet
-- controleren of dit de openings- of sluitings-dollar is. We houden in een
-- extra argument @m@ bij of we in wiskundemodus zitten of niet. Dit helpt ons
-- bij het onderscheid.
stripOpening :: Bool -> String -> Maybe (Symbol, Bool, String)
stripOpening m s
  | Just r <- stripPrefix "$" s      = if m then Nothing else Just (Math, True, r)
  | Just r <- stripPrefix "{" s      = Just (Brace, m, r)
  | Just r <- stripPrefix "[" s      = Just (Bracket, m, r)
  | Just r <- stripPrefix "(" s      = Just (Paren, m, r)
  | Just r <- stripPrefix "\\left" s
  , Just r <- stripDelimiter r       = Just (Delim, m, r)
  | Just r <- stripPrefix "\\start" s
  , (n,r)  <- span isLetter r        = Just (Block n, m, r)
  | Just r <- stripPrefix "\\begin{" s
  , (n,r)  <- span isLetter r
  , Just r <- stripPrefix "}" r      = Just (Env n, m, r)
  | otherwise                        = Nothing

-- | Analoog aan @stripOpening@, maar dan voor sluithaakjes en stop-commando's.
stripClosing :: Bool -> String -> Maybe (Symbol, Bool, String)
stripClosing m s
  | Just r <- stripPrefix "$" s      = if m then Just (Math, False, r) else Nothing
  | Just r <- stripPrefix "}" s      = Just (Brace, m, r)
  | Just r <- stripPrefix "]" s      = Just (Bracket, m, r)
  | Just r <- stripPrefix ")" s      = Just (Paren, m, r)
  | Just r <- stripPrefix "\\right" s
  , Just r <- stripDelimiter r       = Just (Delim, m, r)
  | Just r <- stripPrefix "\\stop" s
  , (n,r)  <- span isLetter r        = Just (Block n, m, r)
  | Just r <- stripPrefix "\\end{" s
  , (n,r)  <- span isLetter r
  , Just r <- stripPrefix "}" r      = Just (Env n, m, r)
  | otherwise                        = Nothing

-- | Controleer of de string begint met een reeks van tekens dat we als haakje
-- kunnen gebruiken voor \left of \right. (Doen we op een creatieve manier
-- waarbij we optimaal gebruik maken van de luiheid van Haskell!)
stripDelimiter :: String -> Maybe String
stripDelimiter s = listToMaybe $ mapMaybe (`stripPrefix` s)
  [ ".", "(", ")", "\\{", "\\}", "[", "]", "<", ">", "|", "\\|", "/"
  , "\\lgroup", "\\rgroup", "\\lbrace", "\\rbrace", "\\langle", "\\rangle"
  , "\\vert", "\\lvert", "\\rvert", "\\Vert", "\\lVert", "\\rVert"
  , "\\backslash", "\\lfloor", "\\rfloor", "\\lceil", "\\rceil"
  , "\\uparrow", "\\Uparrow", "\\downarrow", "\\Downarrow", "\\updownarrow", "\\Updownarrow"
  , "\\llcorner", "\\lrcorner", "\\ulcorner", "\\urconrner"
  , "\\lmoustache", "\\rmoustache" ]

-- | Controleer of de string begint met een nieuwe regel. Strip deze en geef de
-- rest van de string terug.
stripNewLine :: String -> Maybe String
stripNewLine = stripPrefix "\n"

-- | Controleer of de string begint met een procent teken. In dat geval gooien
-- we alle tekens weg tot het einde van de regel, en geven de rest van de string
-- terug.
stripComment :: String -> Maybe String
stripComment s
  | Just r <- stripPrefix "%" s      = Just $ dropWhile (/= '\n') r
  | otherwise                        = Nothing

-- | Controleer of we in een woordelijke omgeving zitten. Alles tussen twee
-- apenstaartjes negeeren we.
stripVerbatim :: String -> Maybe String
stripVerbatim s
  | Just r     <- stripPrefix "@" s          = Just $ tail $ dropWhile (/= '@') r
  -- | Just r     <- stripPrefix "\\starttyping" s = Just $ tail $ dropWhile
  -- | Just r     <- stripPrefix "\\type{" s    = Just $ tail $ dropWhile (/= '}') r
  -- | Just r     <- stripPrefix "\\type(" s    = Just $ tail $ dropWhile (/= ')') r
  -- | Just r     <- stripPrefix "\\type[" s    = Just $ tail $ dropWhile (/= ']') r
  -- | Just r     <- stripPrefix "\\type<" s    = Just $ tail $ dropWhile (/= '>') r
  -- | Just (c:r) <- stripPrefix "\\type"  s    = Just $ tail $ dropWhile (/=  c ) r
  | otherwise                                = Nothing

-- | Controleer of de string begint met een geescaped procent teken, dollar
-- teken of apenstaartje.
-- We willen immers niet dat we over de backslash heen lezen, en vervolgens de
-- procent of het dollar teken aanzien voor commentaar dan wel wiskunde...
stripEscaped :: String -> Maybe String
stripEscaped s = listToMaybe $ mapMaybe (`stripPrefix` s) [ "\\%", "\\$", "\\@"]

-- | Recursief algoritme om te controleren of de string gebalanceerd is
-- wat betreft haakjes en start/stop-paren.
--
-- Een korte omschrijving per regel:
-- * Een lege string met een lege stapel is natuurlijk gebalanceerd.
-- * Een lege string met een niet lege stapel gaat helemaal mis.
-- * In andere gevallen:
--   * We houden het regelnummer bij en hogen het op als we een regeleinde
--     tegenkomen.
--   * We controleren of we met een \% of \$ te maken hebben, dat gooien we weg
--     zodat we verderop niet in de problemen komen.
--   * Commentaar gooien we weg.
--   * Wanneer we een openingshaakje tegenkomen zetten we het huidige
--     regelnummer en het sluithaakje op de stapel.
--   * Wanneer we een sluithaakje tegenkomen controleren we of dit op de stapel
--     stond.
--   * Als de string niet begint met een haakje of start/stop-paar, dan gaan we
--     door met de rest van de tekst.
--
-- Het type van deze functie vraagt eigenlijk om een Writer Monad, maar omdat
-- dit maar een script is, gaan we het ons niet te ingewikkeld maken...
balanced :: Line -> Stack (Line,Symbol) -> Bool -> String -> (Bool,Error)  -- Writer Error Bool
balanced _ []        False  ""    = (True, "")
balanced l ((k,o):_) _      ""    = (False, "Line " ++ show l ++ ":\n   " ++ "Unexpected end of file, expected '" ++ close o ++ "'\n   " ++ "(to match with '" ++ open o ++ "' from line " ++ show k ++ ")")
balanced l os m s
  | Just r       <- stripNewLine s   = balanced (l+1) os m r
  | Just r       <- stripEscaped s   = balanced l os m r
  | Just r       <- stripComment s   = balanced l os m r
  | Just r       <- stripVerbatim s  = balanced l os m r
  | Just (o,m,r) <- stripOpening m s = balanced l ((l,o):os) m r
  | Just (c,m,r) <- stripClosing m s = case os of
    []                              -> (False, "Line " ++ show l ++ ":\n   " ++ "Unexpected '" ++ close c ++ "', closed without opening")
    (k,o) : os  | c == o            -> balanced l os m r
                | otherwise         -> (False, "Line " ++ show l ++ ":\n   " ++ "Unexpected '" ++ close c ++ "', expected '" ++ close o ++ "'\n   " ++ "(to match with '" ++ open o ++ "' from line " ++ show k ++ ")")
  | otherwise                        = balanced l os m $ tail s

-- | Controleer of de string gebalanceerd. We beginnen bij regel 1 met een lege
-- stapel en niet in wiskundemodus.
check :: String -> (Bool,Error)
check = balanced 1 [] False

-- | Lees de tekst in uit een bestand, controleer of deze gebalanceerd is en
-- geef het resultaat door.
run :: FilePath -> IO Bool
run f = do
  putAct f
  s <- readFile f
  case check s of
    (True,_)  -> return True
    (False,e) -> putErr e >> return False

-- | Hoofdfunctie waarbij we de commandoregel argumenten inlezen en controleren
-- of die er überhaupt wel zijn. Vervolgens passen we @run@ toe op elk argument
-- en geven nog een conclusie of het aantal fouten.
main :: IO a
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
    _ -> putInf $ "Oh help, found " ++ show n ++ " errors!"
  exitSuccess

-- vim: nowrap
