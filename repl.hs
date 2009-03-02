{-# OPTIONS_GHC -fglasgow-exts #-}
module Main
  where
import Prelude hiding (read)
import Twerp
import TwerpParser
import IO
import Text.ParserCombinators.Parsec
import Control.Monad.Error

main = do hSetBuffering stdout NoBuffering
          putStr ">> "
          b <- isEOF
          if b then return ()
            else 
              do input <- getLine
                 (putStrLn . unwrap) (toLR (parse snode "interactive shell" input) >>= \val ->
                                   run (stateToEval val))
                 main

toLR (Left er) = fail (show er)
toLR (Right val) = return val

unwrap (Left er) = er
unwrap (Right val) = show val
                     
