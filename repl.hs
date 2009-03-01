{-# OPTIONS_GHC -fglasgow-exts #-}
module Main
  where
import Prelude hiding (read)
import Twerp
import TwerpParser
import IO
import Text.ParserCombinators.Parsec

main = repl
repl = isEOF >>= (\b -> if b then return () else rep >> repl)
rep = read >>= eval >>= print
read = getLine >>= \input -> case parse snode "interactive shell" input of
    (Left err) → return (ErrState (show err))
    (Right s) → return (stateToEval s)
eval = return.run