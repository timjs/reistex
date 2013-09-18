{-# LANGUAGE OverloadedStrings, NamedFieldPuns, DeriveDataTypeable, CPP #-}
module Main where

import Prelude hiding (catch, takeWhile)

import Data.Char
import Data.Monoid
import Data.Typeable

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import Control.Applicative
import Control.Exception
import Control.Monad

import System.Environment
import System.Exit

import Output

#define ATTOPARSEC
#ifdef ATTOPARSEC
import Data.Attoparsec.ByteString.Char8 hiding (isEndOfLine)
#else
import Parser
#endif

-- Allereerst definiëren we een paar type-synoniemen. We gebruiken een
-- eenvoudige lijst als stapel (Stack), een foutmelding is een string en een
-- regelnummer is een geheel getal.
type Stack a = [a]

-- | Een symbool is een abstract data type. Deze kunnen we goedkoper op de
-- stapel zetten dan strings. Een kleine optimalisatie dus. We leiden een
-- instantie voor @Eq@ en @Ord@ af en maken er zelf eentje voor @Show@ aan.
data Symbol  = Brace
             | Bracket
             | Paren
             | Chevron
             | Dollar
             | At
             | Delimiter
             | StartStop Name
             | BeginEnd  Name
             deriving (Show, Eq, Ord)
type Name    = ByteString

openOf :: Symbol -> ByteString
openOf s = case s of
  Brace       -> "{"
  Bracket     -> "["
  Paren       -> "("
  Chevron     -> "<"
  Dollar      -> "$"
  At          -> "@"
  Delimiter   -> "\\left"
  StartStop n -> "\\start" <> n
  BeginEnd  n -> "\\begin{" <> n <> "}"

closeOf :: Symbol -> ByteString
closeOf s = case s of
  Brace       -> "}"
  Bracket     -> "]"
  Paren       -> ")"
  Chevron     -> ">"
  Dollar      -> "$"
  At          -> "@"
  Delimiter   -> "\\right"
  StartStop n -> "\\stop" <> n
  BeginEnd  n -> "\\end{" <> n <> "}"

type Line  = Int
data Mode  = Normal | Math
           deriving (Show, Eq, Ord, Enum)
data State = State { mode  :: Mode
                   , line  :: Line
                   , stack :: Stack (Line,Symbol)
                   } deriving (Show, Eq)

mkState :: State
mkState = State { mode  = Normal
                , line  = 1
                , stack = [] }

data BalancingError = EndOfFile Symbol Line
                    | DoesNotMatch Symbol Line Symbol Line
                    | ClosedWithoutOpening Symbol Line
                    | Unknown String
                    deriving (Typeable)
instance Exception BalancingError

instance Show BalancingError where
  show e = case e of
    EndOfFile s' l'          ->                            "Unexpected end of file, expected '"                       ++ BS.unpack (closeOf s') ++ "'\n   " ++ "(to close '" ++ BS.unpack (openOf s') ++ "' from line " ++ show l' ++ ")"
    DoesNotMatch s l s' l'   -> "Line " ++ show l ++ ":\n   Unexpected '" ++ BS.unpack (closeOf s) ++ "', expected '" ++ BS.unpack (closeOf s') ++ "'\n   " ++ "(to close '" ++ BS.unpack (openOf s') ++ "' from line " ++ show l' ++ ")"
    ClosedWithoutOpening s l -> "Line " ++ show l ++ ":\n   Unexpected '" ++ BS.unpack (closeOf s) ++ "', closed without opening"
    Unknown s                -> "An unknown error occured:\n   " ++ s

-- between :: Char -> Char -> Parser ByteString
-- between o c = char o *> takeTill (== c) <* char c
-- {-# INLINE between #-}

word :: Parser ByteString
word = takeWhile isLetter
{-# INLINE word #-}

isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\n' || c == '\r'
{-# INLINE isEndOfLine #-}

-- skipTill :: (Char -> Bool) -> Parser ()
-- skipTill p = skipWhile (not . p)
-- {-# INLINE skipTill #-}

-- takeWith :: ByteString -> Parser String
-- takeWith s  =  manyTill anyChar (string s)
           -- <|> throw (EndOfVerbatim (ByteString.unpack s))

-- skipWith :: State -> ByteString -> Parser ()
-- skipWith State{line} s  =  (void $ manyTill anyChar (string s))
           -- <|> (throw $ EndOfFile line (ByteString.unpack s))

-- | Controleer of de string begint met een procent teken. In dat geval gooien
-- we alle tekens weg tot het einde van de regel, en geven de rest van de string
-- terug.
comment :: Parser ByteString
comment = char '%' *> takeTill isEndOfLine -- Does not consume end of line character!

-- | Controleer of we in een woordelijke omgeving zitten. Alles tussen twee
-- apenstaartjes negeeren we.
verbatim :: Parser Symbol
verbatim  =  At                 <$ char '@'
         <|> string "\\type" *> choice
               [ Brace          <$ char '{'
               , Bracket        <$ char '['
               , Paren          <$ char '('
               , Chevron        <$ char '<' ]
         <|> StartStop "typing" <$ string "\\starttyping"

-- | Controleer of de string begint met een geescaped procent teken, dollar
-- teken of apenstaartje.
-- We willen immers niet dat we over de backslash heen lezen, en vervolgens de
-- procent of het dollar teken aanzien voor commentaar dan wel wiskunde...
escaped :: Parser ByteString
escaped = choice $ map string ["\\\\", "\\%", "\\$", "\\@"]

-- | Controleer of de string begint met een reeks van tekens dat we als haakje
-- kunnen gebruiken voor \left of \right. (Doen we op een creatieve manier
-- waarbij we optimaal gebruik maken van de luiheid van Haskell!)
delimiter :: Parser ByteString
delimiter = choice $ map string
  [ ".", "(", ")", "[", "]", "<", ">", "|", "/"
  , "\\{", "\\}", "\\|"
  , "\\lgroup", "\\rgroup", "\\lbrace", "\\rbrace", "\\langle", "\\rangle"
  , "\\vert", "\\lvert", "\\rvert", "\\Vert", "\\lVert", "\\rVert"
  , "\\backslash", "\\lfloor", "\\rfloor", "\\lceil", "\\rceil"
  , "\\uparrow", "\\Uparrow", "\\downarrow", "\\Downarrow", "\\updownarrow", "\\Updownarrow"
  , "\\llcorner", "\\lrcorner", "\\ulcorner", "\\urconrner"
  , "\\lmoustache", "\\rmoustache" ]

math :: Parser Symbol
math = Dollar <$ char '$'

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
opening :: Parser Symbol
opening  =  Brace     <$  char '{'
        <|> Bracket   <$  char '['
        <|> Paren     <$  char '('
        <|> Delimiter <$  (string "\\left"   *> delimiter)
        <|> StartStop <$> (string "\\start"  *> word)
        <|> BeginEnd  <$> (string "\\begin{" *> word <* char '}')

-- | Analoog aan @stripOpening@, maar dan voor sluithaakjes en stop-commando's.
closing :: Parser Symbol
closing  =  Brace     <$  char '}'
        <|> Bracket   <$  char ']'
        <|> Paren     <$  char ')'
        <|> Delimiter <$  (string "\\right" *> delimiter)
        <|> StartStop <$> (string "\\stop"  *> word)
        <|> BeginEnd  <$> (string "\\end{"  *> word <* char '}')

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
balanced :: Parser Bool
balanced = go mkState where
    go st  =  (endOfInput >>  end st)
          <|> (endOfLine  >>  increase st >>= go)
          <|> (escaped    >>  go st)
          <|> (comment    >>  go st)
          <|> (verbatim   >>= skip st >>= go)
          <|> (math       >>= decide st >>= go)
          <|> (opening    >>= push st >>= go)
          <|> (closing    >>= pop st >>= go)
          <|> (anyChar    >>  go st)

end :: State -> Parser Bool
end State{stack=(line,open):_} = throw $ EndOfFile open line
end _                          = return True

increase :: State -> Parser State
increase st@State{line} = return st{line = line + 1}

decide :: State -> Symbol -> Parser State
decide st@State{mode} = case mode of
  Math   -> pop st{mode = Normal}
  Normal -> push st{mode = Math}

push :: State -> Symbol -> Parser State
push st@State{stack,line} open = return st{stack = (line, open) : stack}

pop :: State -> Symbol -> Parser State
pop st@State{stack,line} close = case stack of
  []                   -> throw $ ClosedWithoutOpening close line
  (line',open):rest
    | close == open    -> return st{stack = rest}
    | otherwise        -> throw $ DoesNotMatch close line open line'

skip :: State -> Symbol -> Parser State
skip st@State{line} y  =  st <$ anyChar `manyTill` string (closeOf y)
                      <|> throw (EndOfFile y line)

-- | Lees de tekst in uit een bestand, controleer of deze gebalanceerd is en
-- geef het resultaat door.
run :: FilePath -> IO Bool
run f = do
  putAct f
  s <- BS.readFile f
  case parseOnly balanced s of
    Right b -> return b
    Left  m -> throw $ Unknown m
  `catch` \e -> do
    putErr $ show (e :: BalancingError)
    return False

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
