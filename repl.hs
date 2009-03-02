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
rep = read >>= eval >>= putStrLn
read = getLine >>= \input -> case parse snode "interactive shell" input of
    (Left err) → return (ErrState (show err))
    (Right s) → return (stateToEval s)
eval v = case run v of
  Left err -> return err
  Right val -> return (show val)