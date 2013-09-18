{-# LANGUAGE OverloadedStrings, NamedFieldPuns, DeriveDataTypeable #-}
module Main where

import Prelude hiding (catch)

import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Typeable

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import Control.Exception
import Control.Monad

import System.Environment
import System.Exit

import Output

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
                    deriving (Typeable)
instance Exception BalancingError

instance Show BalancingError where
  show e = case e of
    EndOfFile s' l'          ->                            "Unexpected end of file, expected '"                       ++ BS.unpack (closeOf s') ++ "'\n   " ++ "(to close '" ++ BS.unpack (openOf s') ++ "' from line " ++ show l' ++ ")"
    DoesNotMatch s l s' l'   -> "Line " ++ show l ++ ":\n   Unexpected '" ++ BS.unpack (closeOf s) ++ "', expected '" ++ BS.unpack (closeOf s') ++ "'\n   " ++ "(to close '" ++ BS.unpack (openOf s') ++ "' from line " ++ show l' ++ ")"
    ClosedWithoutOpening s l -> "Line " ++ show l ++ ":\n   Unexpected '" ++ BS.unpack (closeOf s) ++ "', closed without opening"

stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix p s
  | p `BS.isPrefixOf` s = Just $ BS.drop (BS.length p) s
  | otherwise           = Nothing

stripInfix :: ByteString -> ByteString -> Maybe ByteString
stripInfix _ ""        = Nothing
stripInfix i s
 | i `BS.isPrefixOf` s = Just $ BS.drop (BS.length i) s
 | otherwise           = stripInfix i (BS.tail s)

-- | Controleer of de string begint met een procent teken. In dat geval gooien
-- we alle tekens weg tot het einde van de regel, en geven de rest van de string
-- terug.
comment :: ByteString -> Maybe ByteString
comment s
  | Just r <- stripPrefix "%" s      = Just $ BS.dropWhile (/= '\n') r
  | otherwise                        = Nothing

-- | Controleer of we in een woordelijke omgeving zitten. Alles tussen twee
-- apenstaartjes negeeren we.
verbatim :: ByteString -> Maybe (Symbol,ByteString)
verbatim s
  | Just r     <- stripPrefix "@" s         = Just (At, r)
  | Just r     <- stripPrefix "\\type" s
  , Just (c,r) <- BS.uncons r               = case c of
                                                '{' -> Just (Brace, r)
                                                '[' -> Just (Bracket, r)
                                                '(' -> Just (Paren, r)
                                                '<' -> Just (Chevron, r)
                                                _   -> Nothing -- returns here, does not check next choices!
  | Just r <- stripPrefix "\\starttyping" s = Just (StartStop "typing", r)
  | otherwise                               = Nothing

-- | Controleer of de string begint met een geescaped procent teken, dollar
-- teken of apenstaartje.
-- We willen immers niet dat we over de backslash heen lezen, en vervolgens de
-- procent of het dollar teken aanzien voor commentaar dan wel wiskunde...
escaped :: ByteString -> Maybe ByteString
escaped s = listToMaybe $ mapMaybe (`stripPrefix` s) ["\\\\", "\\%", "\\$", "\\@"]

-- | Controleer of de string begint met een reeks van tekens dat we als haakje
-- kunnen gebruiken voor \left of \right. (Doen we op een creatieve manier
-- waarbij we optimaal gebruik maken van de luiheid van Haskell!)
delimiter :: ByteString -> Maybe ByteString
delimiter s = listToMaybe $ mapMaybe (`stripPrefix` s)
  [ ".", "(", ")", "[", "]", "<", ">", "|", "/"
  , "\\{", "\\}", "\\|"
  , "\\lgroup", "\\rgroup", "\\lbrace", "\\rbrace", "\\langle", "\\rangle"
  , "\\vert", "\\lvert", "\\rvert", "\\Vert", "\\lVert", "\\rVert"
  , "\\backslash", "\\lfloor", "\\rfloor", "\\lceil", "\\rceil"
  , "\\uparrow", "\\Uparrow", "\\downarrow", "\\Downarrow", "\\updownarrow", "\\Updownarrow"
  , "\\llcorner", "\\lrcorner", "\\ulcorner", "\\urconrner"
  , "\\lmoustache", "\\rmoustache" ]

math :: ByteString -> Maybe (Symbol,ByteString)
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
opening :: ByteString -> Maybe (Symbol,ByteString)
opening s
  | Just r <- stripPrefix "{" s      = Just (Brace, r)
  | Just r <- stripPrefix "[" s      = Just (Bracket, r)
  | Just r <- stripPrefix "(" s      = Just (Paren, r)
  | Just r <- stripPrefix "\\left" s
  , Just r <- delimiter r            = Just (Delimiter, r)
  | Just r <- stripPrefix "\\start" s
  , (n,r)  <- BS.span isLetter r     = Just (StartStop n, r)
  | Just r <- stripPrefix "\\begin{" s
  , (n,r)  <- BS.span isLetter r
  , Just r <- stripPrefix "}" r      = Just (BeginEnd n, r)
  | otherwise                        = Nothing

-- | Analoog aan @opening@, maar dan voor sluithaakjes en stop-commando's.
closing :: ByteString -> Maybe (Symbol,ByteString)
closing s
  | Just r <- stripPrefix "}" s      = Just (Brace, r)
  | Just r <- stripPrefix "]" s      = Just (Bracket, r)
  | Just r <- stripPrefix ")" s      = Just (Paren, r)
  | Just r <- stripPrefix "\\right" s
  , Just r <- delimiter r            = Just (Delimiter, r)
  | Just r <- stripPrefix "\\stop" s
  , (n,r)  <- BS.span isLetter r     = Just (StartStop n, r)
  | Just r <- stripPrefix "\\end{" s
  , (n,r)  <- BS.span isLetter r
  , Just r <- stripPrefix "}" r      = Just (BeginEnd n, r)
  | otherwise                        = Nothing

-- | Controleer of de string begint met een nieuwe regel. Strip deze en geef de
-- rest van de string terug.
newLine :: ByteString -> Maybe ByteString
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
balanced :: ByteString -> Bool
balanced s = go s mkState where
  go "" st                     = end st
  go s  st
    | Just r     <- newLine s  = go r $ increase st
    | Just r     <- escaped s  = go r st
    | Just r     <- comment s  = go r st
    | Just (d,r) <- verbatim s = uncurry go $ skip r d st
    | Just (d,r) <- math s     = go r $ decide d st
    | Just (o,r) <- opening s  = go r $ push o st
    | Just (c,r) <- closing s  = go r $ pop c st
    | otherwise                = go (BS.tail s) st

end :: State -> Bool
end State{stack=(line,open):_} = throw $ EndOfFile open line
end _                          = True

increase :: State -> State
increase st@State{line} = st{line = line + 1}

decide :: Symbol -> State -> State
decide y st@State{mode} = case mode of
  Math   -> pop y st{mode = Normal}
  Normal -> push y st{mode = Math}

push :: Symbol -> State -> State
push open st@State{stack,line} = st{stack = (line, open) : stack}

pop :: Symbol -> State -> State
pop close st@State{stack,line} = case stack of
  []                   -> throw $ ClosedWithoutOpening close line
  (line',open):rest
    | close == open    -> st{stack = rest}
    | otherwise        -> throw $ DoesNotMatch close line open line'

skip :: ByteString -> Symbol -> State -> (ByteString, State)
skip s y st@State{line}
  | Just r <- stripInfix (closeOf y) s = (r, st)
  | otherwise                          = throw $ EndOfFile y line

-- | Lees de tekst in uit een bestand, controleer of deze gebalanceerd is en
-- geef het resultaat door.
run :: FilePath -> IO Bool
run f = do
  putAct f
  s <- BS.readFile f
  evaluate (balanced s) `catch` \e -> do
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
