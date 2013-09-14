{-# LANGUAGE OverloadedStrings, NamedFieldPuns, CPP #-}
module Main where

import Data.Char
import Data.Maybe

import Control.Monad

import System.Environment
import System.Exit

import Output

#define BYTESTRING
#define LAZY

#if defined TEXT
#if defined LAZY
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
#else
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
#endif
unpack = T.unpack
stripPrefix = T.stripPrefix

#elif defined BYTESTRING
#if defined LAZY
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as T
#else
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as T
#endif
type Text = ByteString
unpack = T.unpack
stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix p s
  | p `T.isPrefixOf` s = Just $ T.drop (T.length p) s
  | otherwise          = Nothing
stripInfix :: ByteString -> ByteString -> ByteString
stripInfix _ "" = ""
stripInfix i s
 | i `T.isPrefixOf` s  = T.drop (T.length i) s
 | otherwise           = stripInfix i (T.tail s)

#elif defined STRING
import qualified Data.List as T
import qualified System.IO as T
type Text = String
stripPrefix = T.stripPrefix
unpack = id
#endif

-- Allereerst definiëren we een paar type-synoniemen. We gebruiken een
-- eenvoudige lijst als stapel (Stack), een foutmelding is een string en een
-- regelnummer is een geheel getal.
type Stack a = [a]
type Error   = String
type Position= Int

-- | Een symbool is een abstract data type. Deze kunnen we goedkoper op de
-- stapel zetten dan strings. Een kleine optimalisatie dus. We leiden een
-- instantie voor @Eq@ en @Ord@ af en maken er zelf eentje voor @Show@ aan.
data Symbol  = Brace
             | Bracket
             | Paren
             | Dollar
             | At
             | Delimiter
             | StartStop Name
             | BeginEnd  Name
             deriving (Show, Eq, Ord)
type Name    = Text

showOpen :: Symbol -> String
showOpen s = case s of
  Brace       -> "{"
  Bracket     -> "["
  Paren       -> "("
  Dollar      -> "$"
  At          -> "@"
  Delimiter   -> "\\left"
  StartStop n -> "\\start" ++ unpack n
  BeginEnd n  -> "\\begin{" ++ unpack n ++ "}"

showClose :: Symbol -> String
showClose s = case s of
  Brace       -> "}"
  Bracket     -> "]"
  Paren       -> ")"
  Dollar      -> "$"
  At          -> "@"
  Delimiter   -> "\\right"
  StartStop n -> "\\stop" ++ unpack n
  BeginEnd n  -> "\\end{" ++ unpack n ++ "}"

data Mode  = Normal
           | Math
           | Verbatim
           deriving (Show, Eq, Ord, Enum)
data State = State { mode     :: Mode
                   , position :: Position
                   , stack    :: Stack (Position,Symbol)
                   } deriving (Show, Eq)

mkState :: State
mkState = State { mode     = Normal
                , position = 1
                , stack    = [] }

-- | Controleer of de string begint met een procent teken. In dat geval gooien
-- we alle tekens weg tot het einde van de regel, en geven de rest van de string
-- terug.
comment :: Text -> Maybe Text
comment s
  | Just r <- stripPrefix "%" s      = Just $ T.dropWhile (/= '\n') r
  | otherwise                        = Nothing

-- | Controleer of de string begint met een geescaped procent teken, dollar
-- teken of apenstaartje.
-- We willen immers niet dat we over de backslash heen lezen, en vervolgens de
-- procent of het dollar teken aanzien voor commentaar dan wel wiskunde...
escaped :: Text -> Maybe Text
escaped s = listToMaybe $ mapMaybe (`stripPrefix` s) [ "\\%", "\\$", "\\@"]

-- | Controleer of de string begint met een reeks van tekens dat we als haakje
-- kunnen gebruiken voor \left of \right. (Doen we op een creatieve manier
-- waarbij we optimaal gebruik maken van de luiheid van Haskell!)
delimiter :: Text -> Maybe Text
delimiter s = listToMaybe $ mapMaybe (`stripPrefix` s)
  [ ".", "(", ")", "\\{", "\\}", "[", "]", "<", ">", "|", "\\|", "/"
  , "\\lgroup", "\\rgroup", "\\lbrace", "\\rbrace", "\\langle", "\\rangle"
  , "\\vert", "\\lvert", "\\rvert", "\\Vert", "\\lVert", "\\rVert"
  , "\\backslash", "\\lfloor", "\\rfloor", "\\lceil", "\\rceil"
  , "\\uparrow", "\\Uparrow", "\\downarrow", "\\Downarrow", "\\updownarrow", "\\Updownarrow"
  , "\\llcorner", "\\lrcorner", "\\ulcorner", "\\urconrner"
  , "\\lmoustache", "\\rmoustache" ]

-- | Controleer of we in een woordelijke omgeving zitten. Alles tussen twee
-- apenstaartjes negeeren we.
verbatim :: Text -> Maybe (Symbol,Text)
verbatim s
  | Just r <- stripPrefix "@" s      = Just (At, r)
  | otherwise                        = Nothing

math :: Text -> Maybe (Symbol,Text)
math s
  | Just r <- stripPrefix "$" s      = Just (Dollar, r)
  | otherwise                        = Nothing

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
opening :: Text -> Maybe (Symbol,Text)
opening s
  | Just r <- stripPrefix "{" s      = Just (Brace, r)
  | Just r <- stripPrefix "[" s      = Just (Bracket, r)
  | Just r <- stripPrefix "(" s      = Just (Paren, r)
  | Just r <- stripPrefix "\\left" s
  , Just r <- delimiter r            = Just (Delimiter, r)
  | Just r <- stripPrefix "\\start" s
  , (n,r)  <- T.span isLetter r      = Just (StartStop n, r)
  | Just r <- stripPrefix "\\begin{" s
  , (n,r)  <- T.span isLetter r
  , Just r <- stripPrefix "}" r      = Just (BeginEnd n, r)
  | otherwise                        = Nothing

-- | Analoog aan @opening@, maar dan voor sluithaakjes en stop-commando's.
closing :: Text -> Maybe (Symbol,Text)
closing s
  | Just r <- stripPrefix "}" s      = Just (Brace, r)
  | Just r <- stripPrefix "]" s      = Just (Bracket, r)
  | Just r <- stripPrefix ")" s      = Just (Paren, r)
  | Just r <- stripPrefix "\\right" s
  , Just r <- delimiter r            = Just (Delimiter, r)
  | Just r <- stripPrefix "\\stop" s
  , (n,r)  <- T.span isLetter r      = Just (StartStop n, r)
  | Just r <- stripPrefix "\\end{" s
  , (n,r)  <- T.span isLetter r
  , Just r <- stripPrefix "}" r      = Just (BeginEnd n, r)
  | otherwise                        = Nothing

-- | Controleer of de string begint met een nieuwe regel. Strip deze en geef de
-- rest van de string terug.
newLine :: Text -> Maybe Text
newLine = stripPrefix "\n"

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
balanced :: Text -> Either Error State
balanced s = go s mkState where
  go "" st                     = end st
  go s  st
    | Just r     <- newLine s  = increase st >>= go r
    | Just r     <- escaped s  = go r st
    | Just r     <- comment s  = go r st
    | Just (m,r) <- verbatim s = decide m st >>= go r
    | Just (m,r) <- math s     = decide m st >>= go r
    | Just (o,r) <- opening s  = push o st >>= go r
    | Just (c,r) <- closing s  = pop c st >>= go r
    | otherwise                = go (T.tail s) st

end :: State -> Either Error State
end State{stack=(line,open):_,position} = Left $ "Line " ++ show position ++ ":\n   " ++ "Unexpected end of file, expected '" ++ showClose open ++ "'\n   " ++ "(to match with '" ++ showOpen open ++ "' at line " ++ show line ++ ")"
end st                                  = Right st

increase :: State -> Either Error State
increase state@State{position} = Right state{position = position + 1}

switch :: Mode -> State -> Either Error State
switch mode state = Right $ state{mode=mode}

decide :: Symbol -> State -> Either Error State
decide Dollar state@State{mode} = case mode of
  Math     -> pop  Dollar state >>= switch Normal
  Normal   -> push Dollar state >>= switch Math
  Verbatim -> Right state
decide At state@State{mode} = case mode of
  Verbatim -> pop  At state >>= switch Normal
  _        -> push At state >>= switch Verbatim

push :: Symbol -> State -> Either Error State
push open state@State{stack,position,mode} = case mode of
  Verbatim -> Right state -- Don't push anything if we are in verbatim mode
  _        -> Right state{stack = (position, open) : stack}

pop :: Symbol -> State -> Either Error State
pop close state@State{stack,position,mode} = case mode of
  Verbatim -> Right state -- Don't pop anything if we are in verbatim mode
  _        -> case stack of
    []                   -> Left $ "Line " ++ show position ++ ":\n   " ++ "Unexpected '" ++ showClose close ++ "', closed without opening"
    (line,open):rest
      | close == open    -> Right state{stack = rest}
      | otherwise        -> Left $ "Line " ++ show position ++ ":\n   " ++ "Unexpected '" ++ showClose close ++ "', expected '" ++ showClose open ++ "'\n   " ++ "(to match with '" ++ showOpen open ++ "' at line " ++ show line ++ ")"

-- | Lees de tekst in uit een bestand, controleer of deze gebalanceerd is en
-- geef het resultaat door.
run :: FilePath -> IO Bool
run f = do
  putAct f
  s <- T.readFile f
  case balanced s of
    Right _ -> return True
    Left e  -> putErr e >> return False

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
  let n = length . filter (== False) $ rs
  case n of
    0 -> putInf "Everything seems to be all right!"
    1 -> putInf "Oops, found 1 error!"
    _ -> putInf $ "Oh help, found " ++ show n ++ " errors!"
  exitSuccess

-- vim: nowrap
