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
          <|> (char '\\' >> anyChar)

list :: Parser SNode
list = do char '('
          l <- many (snode >>= \x -> spaces >> return x)
          char ')'
          return (SList l)

quotelist = do char '\''
               (SList l) <- list
               return $ SList $ Symbol "quote":l

snode :: Parser SNode
snode = atom <|> list <|> quotelist

parse inputName input = case Parsec.parse snode inputName input of
    (Left err) -> fail $ show err
    (Right s) -> return s