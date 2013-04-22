module Main where

import Prelude hiding (product)
import Text.Parsec
import Text.Parsec.String

letters :: Parser String
letters     = many letter

start, stop :: Parser String
start = string "\\start"
stop  = string "\\stop"

parens, braces, brackets :: Parser a -> Parser a
parens      = between (char '(') (char ')')
braces      = between (char '{') (char '}')
brackets    = between (char '[') (char ']')

startstop :: Parser a -> Parser b -> Parser b
startstop s = between (start >> s) (stop >> s)

{-structure :: String -> Parser a -> Parser a
structure s = between (string s) (stop >> string s)-}

text, project, product, component, environment :: Parser a -> Parser a
text        = startstop $ string "text"
project     = startstop $ string "project"
product     = startstop $ string "product"
component   = startstop $ string "component"
environment = startstop $ string "environment"

file :: Parser String
file =   try (text content)
     <|> try (project content)
     <|> try (product content)
     <|> try (component content)
     <|> try (environment content)
{-file = do
  start
  name <- letters
  content
  stop
  string name
file  =  start
     >>( text content
     <|> project content
     <|> product content
     <|> component content
     <|> environment content )-}

content :: Parser String
content = parens content
        <|> braces content
        <|> brackets content
        -- <|> startstop letters content
        <|> (many (noneOf "{[()]}") >> content)

{-content :: Parser [String]
content  =  many content'
  where content'  =  parens content'
                 <|> braces content'
                 <|> brackets content'
                 -- <|> startstop letters content
                 <|> many1 letter-}

