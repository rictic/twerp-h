{-# OPTIONS_GHC -fglasgow-exts #-}
module Main
  where
import Prelude hiding (read)
import Twerp
import IO
import Control.Monad.Error

main = do hSetBuffering stdout NoBuffering
          repl

repl = do putStr ">> "
          b <- isEOF
          if b 
            then return () -- all done
            else 
              do input <- getLine
                 (putStrLn . unwrap) (evalFrom "interactive shell" input)
                 repl

unwrap (Left er) = er
unwrap (Right val) = show val
                     
