module Twerp
  where
import TwerpParser
import TwerpInterp

eval :: Monad m => String -> m SNode
eval = evalFrom "Twerp"
evalFrom source input = parse source input >>= run . stateToEval