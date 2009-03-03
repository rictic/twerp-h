{-# OPTIONS_GHC -fglasgow-exts #-}
module TwerpParser
  where
import TwerpInterp
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec

atom :: Parser SNode
atom = Symbol <$> (quotedAtom <|> many1 escapedChar)

quotedAtom = do char '"'
                a <- many (noneOf "\"")
                char '"' 
                return a
escapedChar = letter 
          <|> digit
          <|> char '#'
          <|> (char '\\' >> anyChar)

list :: Parser SNode
list = do char '('
          whitespace
          l <- many (snode >>= \x -> whitespace >> return x)
          char ')'
          return (SList l)

quotelist = do char '\''
               s <- snode
               return $ SList [Symbol "quote", s]


snode :: Parser SNode
snode = whitespace >> (atom <|> list <|> quotelist) >>= \x -> whitespace >> return x

skip p = p >> return ()
whitespace = skip $ many $ comment <|> skip (oneOf " \t\r\n")
comment = do char ';'
             many (noneOf "\n")
             ((char '\n' >> return ()) <|> eof)


parse inputName input = case Parsec.parse snode inputName input of
    (Left err) -> fail $ show err
    (Right s) -> return s