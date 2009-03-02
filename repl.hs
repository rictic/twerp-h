{-# OPTIONS_GHC -fglasgow-exts #-}
module Main
  where
import Prelude hiding (read)
import TwerpInterp
import TwerpParser
import IO
import Control.Monad.Error

main = do hSetBuffering stdout NoBuffering
          putStr ">> "
          b <- isEOF
          if b then return ()
            else 
              do input <- getLine
                 (putStrLn . unwrap) ((parse "interactive shell" input) >>= (run . stateToEval))
                 main

unwrap (Left er) = er
unwrap (Right val) = show val
                     
