{-# OPTIONS_GHC -fglasgow-exts #-}
module TwerpParser
  where
import TwerpInterp
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec

atom :: Parser SNode
atom = Symbol <$> (doubleQuoteAtom <|> many1 escapedChar)

doubleQuoteAtom = do char '"'
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

quoted = do char '\''
            s <- snode
            return $ SList [Symbol "quote", s]


snode :: Parser SNode
snode = whitespace >> (atom <|> list <|> quoted) >>= \x -> whitespace >> return x

skip p = p >> return ()
whitespace = skip $ many $ comment <|> skip (oneOf " \t\r\n")
comment = do char ';'
             many (noneOf "\n")
             ((char '\n' >> return ()) <|> eof)


parse inputName input = case Parsec.parse snode inputName input of
    (Left err) -> fail $ show err
    (Right s) -> return s