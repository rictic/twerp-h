{-# OPTIONS_GHC -fglasgow-exts #-}
module TwerpParser
  where
import Twerp
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec


atom :: Parser SNode
atom = Symbol <$> (quotedAtom <|> many1 escapedChar)

quotedAtom = do char '"'
                a <- many (noneOf "\"")
                char '"' 
                return a
escapedChar = letter 
          <|> digit
          <|> (char '\\' >> anyChar)

list :: Parser SNode
list = do char '('
          l <- many (snode >>= \x -> spaces >> return x)
          char ')'
          return (SList l)

snode :: Parser SNode
snode = atom <|> list
