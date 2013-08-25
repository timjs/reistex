{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Main where

import Prelude hiding (takeWhile) --TODO

import Data.Char
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Attoparsec.Text

import Control.Applicative
import Control.Monad

import System.Environment
import System.Exit

import Output

-- Allereerst definiëren we een paar type-synoniemen. We gebruiken een
-- eenvoudige lijst als stapel (Stack), een foutmelding is een string en een
-- regelnummer is een geheel getal.
type Stack a  = [a]
type Error    = Text
type Position = Int

-- | Een symbool is een abstract data type. Deze kunnen we goedkoper op de
-- stapel zetten dan strings. Een kleine optimalisatie dus. We leiden een
-- instantie voor @Eq@ en @Ord@ af en maken er zelf eentje voor @Show@ aan.
data Symbol  = Brace
             | Bracket
             | Paren
             | Dollar
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
  Delimiter   -> "\\left"
  StartStop n -> "\\start" ++ show n
  BeginEnd  n -> "\\begin{" ++ show n ++ "}"

showClose :: Symbol -> String
showClose s = case s of
  Brace       -> "}"
  Bracket     -> "]"
  Paren       -> ")"
  Dollar      -> "$"
  Delimiter   -> "\\right"
  StartStop n -> "\\stop" ++ show n
  BeginEnd  n -> "\\end{" ++ show n ++ "}"

data State = State { inMath     :: Bool
                   , position   :: Position
                   , stack      :: Stack (Position,Symbol)
                   } deriving (Show, Eq)

mkState :: State
mkState = State { inMath = False
                 , position   = 1
                 , stack  = [] }

-- | /O(n)/ Bereken hoe vaak @x@ in de lijst voorkomt.
aantal :: (Eq a) => a -> [a] -> Int --TODO
aantal _ []     = 0
aantal x (y:ys)
  | x == y     = 1 + aantal x ys
  | otherwise  = aantal x ys
-- aantal x = length . filter (== x) -- maar is O(n+m)

between :: Char -> Char -> Parser Text
between o c = char o *> takeTill (== c) <* char c

word :: Parser Text
word = takeWhile isLetter

backslash, dollar, percent, at :: Parser Char
backslash = char '\\'
dollar    = char '$'
percent   = char '%'
at        = char '@'


-- | Controleer of de string begint met een procent teken. In dat geval gooien
-- we alle tekens weg tot het einde van de regel, en geven de rest van de string
-- terug.
comment :: Parser Text
comment = percent *> takeTill isEndOfLine -- Does not consume end of line character!

-- | Controleer of we in een woordelijke omgeving zitten. Alles tussen twee
-- apenstaartjes negeeren we.
verbatim :: Parser Text
verbatim  =  between '@' '@'
         -- <|> backslash *> string "starttyping"
         <|> backslash *> string "type" *> inner
         where inner  =  between '{' '}'
                     <|> between '[' ']'
                     <|> between '(' ')'
                     <|> between '<' '>'
                     -- <|> between  c   c --TODO

-- | Controleer of de string begint met een geescaped procent teken, dollar
-- teken of apenstaartje.
-- We willen immers niet dat we over de backslash heen lezen, en vervolgens de
-- procent of het dollar teken aanzien voor commentaar dan wel wiskunde...
escaped :: Parser Char
escaped = choice $ map (backslash *>) [backslash, percent, dollar, at]
--escaped = choice $ map string ["\\\\", "\\%", "\\$", "\\@"]

-- | Controleer of de string begint met een reeks van tekens dat we als haakje
-- kunnen gebruiken voor \left of \right. (Doen we op een creatieve manier
-- waarbij we optimaal gebruik maken van de luiheid van Haskell!)
delimiter :: Parser Text
delimiter = choice $
  map string [".", "(", ")", "[", "]", "<", ">", "|", "/"] ++
  map (\t -> backslash *> string t)
    [ "{", "}", "|"
    , "lgroup", "rgroup", "lbrace", "rbrace", "langle", "rangle"
    , "vert", "lvert", "rvert", "Vert", "lVert", "rVert"
    , "backslash", "lfloor", "rfloor", "lceil", "rceil"
    , "uparrow", "Uparrow", "downarrow", "Downarrow", "updownarrow", "Updownarrow"
    , "llcorner", "lrcorner", "ulcorner", "urconrner"
    , "lmoustache", "rmoustache" ]

math :: Parser Symbol
math = Dollar <$ dollar

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
        <|> Delimiter <$  (backslash *> string "left"  *> delimiter)
        <|> StartStop <$> (backslash *> string "start" *> word)
        <|> BeginEnd  <$> (backslash *> string "begin" *> word)

-- | Analoog aan @stripOpening@, maar dan voor sluithaakjes en stop-commando's.
closing :: Parser Symbol
closing  =  Brace     <$  char '}'
        <|> Bracket   <$  char ']'
        <|> Paren     <$  char ')'
        <|> Delimiter <$  (backslash *> string "right" *> delimiter)
        <|> StartStop <$> (backslash *> string "stop"  *> word)
        <|> BeginEnd  <$> (backslash *> string "end"   *> word)

infixr 1 <<

(<<) :: Monad m => m b -> m a -> m b
(<<) = flip (>>)

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
balanced = go mkState
  where
    go st  =  (go  =<< increase st   << endOfLine)
          <|> (go  =<< st            <$ escaped)
          <|> (go  =<< st            <$ comment)
          <|> (go  =<< st            <$ verbatim)
          <|> (go  =<< popOrPush st =<< math)
          <|> (go  =<< push st      =<< opening)
          <|> (go  =<< pop st       =<< closing)
          <|> (go  =<< st            <$ anyChar)
          <|> (end =<< st            <$ endOfInput)
    end State{stack=(line,open):_,position} = fail $ "Line " ++ show position ++ ":\n   " ++ "Unexpected end of file, expected '" ++ showClose open ++ "'\n   " ++ "(to match with '" ++ showOpen open ++ "' from line " ++ show line ++ ")"
    end _                                      = return True
{-
    go st  =  (go  =<< increase st  <$  endOfLine)
          <|> (go  =<< st           <$  escaped)
          <|> (go  =<< st           <$  comment)
          <|> (go  =<< st           <$  verbatim)
          <|> (go  =<< popOrPush st <$> math)
          <|> (go  =<< push st      <$> opening)
          <|> (go  =<< pop st       <$> closing)
          <|> (go  =<< st           <$  anyChar)
          <|> (end =<< st           <$  endOfInput)
    end State{stack=(line,open):rest,position} = fail $ "Line " ++ show position ++ ":\n   " ++ "Unexpected end of file, expected '" ++ showClose open ++ "'\n   " ++ "(to match with '" ++ showOpen open ++ "' from line " ++ show line ++ ")"
    end _                                      = return True
-}

increase :: Monad m => State -> m State
increase state@State{position} = return state{position = position + 1}

popOrPush :: Monad m => State -> Symbol -> m State
popOrPush state symbol
  | inMath state = pop state{inMath = False} symbol
  | otherwise    = push state{inMath = True} symbol

push :: Monad m => State -> Symbol -> m State
push state@State{stack,position} open = return state{stack = (position, open) : stack}

pop :: Monad m => State -> Symbol -> m State
pop state@State{stack,position} close = case stack of
  []                   -> fail $ "Line " ++ show position ++ ":\n   " ++ "Unexpected '" ++ showClose close ++ "', closed without opening"
  (line,open):rest
    | close == open    -> return state{stack = rest}
    | otherwise        -> fail $ "Line " ++ show position ++ ":\n   " ++ "Unexpected '" ++ showClose close ++ "', expected '" ++ showClose open ++ "'\n   " ++ "(to match with '" ++ showOpen open ++ "' from Position " ++ show line ++ ")"

-- | Lees de tekst in uit een bestand, controleer of deze gebalanceerd is en
-- geef het resultaat door.
run :: FilePath -> IO Bool
run f = do
  putAct f
  t <- Text.readFile f
  case parseOnly balanced t of
    Right bool -> return bool
    Left  mesg -> putErr mesg >> return False

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
  let n = aantal False rs
  case n of
    0 -> putInf "Everything seems to be all right!"
    1 -> putInf "Oops, found 1 error!"
    _ -> putInf $ "Oh help, found " ++ show n ++ " errors!"
  exitSuccess

-- vim: nowrap
