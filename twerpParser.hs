{-# OPTIONS_GHC -fglasgow-exts #-}
module TwerpParser
  where
import Twerp
import Text.ParserCombinators.Parsec
import Control.Monad

atom :: Parser SNode
atom = liftM (Symbol) (quotedAtom <|> many1 escapedChar)

quotedAtom = do char '"'
                a <- many (noneOf "\"")
                char '"' 
                return a
escapedChar = letter

list :: Parser SNode
list = do char '('
          l <- many (snode >>= \x -> spaces >> return x)
          char ')'
          return (SList l)

snode :: Parser SNode
snode = atom <|> list
