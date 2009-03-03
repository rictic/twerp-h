{-# OPTIONS_GHC -fglasgow-exts #-}
module TwerpInterp
  where
import Prelude hiding (lookup)
import qualified Prelude as P
import Control.Monad.Error

type Primtype = State -> SNode -> (SNode,State)
data SNode = SList [SNode] | Symbol String deriving Eq
data State = State { env :: [(String, SNode)], 
                       work :: [Work], 
                       stack :: [SNode], 
                       intern :: [(String, SNode)] } deriving Show
data Work = Eval SNode | Bind String | Unbind String | Apply [SNode] | If deriving (Show, Eq)

instance Show SNode where
  show (Symbol s) = s
  show (SList []) = "nil"
  show (SList l) = "(" ++ f l ++ ")"
        where f [x] = show x
              f (x:xs) = show x ++ " " ++ f xs

nil = SList []
true = Symbol "#t"
false = Symbol "#f"
prim a = (a, SList [Symbol "prim", Symbol a])
primitives = map prim ["car", "cdr", "cons", "explode", "implode", "null", "atomp"]
builtins = [("nil",nil)] ++ primitives
minSt = State {env = [], work = [], stack = [], intern = builtins}
stateToEval :: SNode -> State
stateToEval initialExpr = minSt {work = [Eval initialExpr]}

step :: Monad m => State -> m State
step st@State {work=w:ws} = step' w $ st {work=ws}

step' :: Monad m => Work -> State -> m State
step' (Eval (Symbol x)) st@State {env = e, stack=s, intern=i} = do v <- lookup x (e++i)
                                                                   return $ st {stack=v:s}
step' (Bind x) st@State {env = e, stack=val:s} = return $ st {env=(x,val):e, stack=s}
step' (Unbind x) st@State {env = (k,v):e} = if k == x 
                                  then return $ st {env=e}
                                  else fail ("Expected unbind " ++ x ++ " found " ++ k)
step' (Apply argC) st@State {stack=s, work=w} = case func of
                              SList ((Symbol "lambda"):params:[body]) -> return $ st {work=addLambdaWork st params body ++ w, stack=reverse args}
                              SList ((Symbol "prim"):[Symbol p]) -> do r <- primCall p args 
                                                                       return $ st {stack=r:stk}
                              errval -> fail $ "tried to apply " ++ show func ++ " to args " ++ show args
                          where (argsr,stk) = splitAt (length argC) s
                                func:args = reverse argsr
                                addLambdaWork st (SList params) body = map (Bind . show) params ++ [Eval body] ++ map (Unbind . show) (reverse params)
step' (Eval (SList l)) st = stepFunCall l st
step' If st@State {stack=p:thenV:elseV:s, work=ws} = return $ st {stack=s, work=(Eval val):ws}
                                      where val = if p == Symbol "#t" then thenV else elseV
step' work st = fail $ "can't perform " ++ show work ++ " with state: " ++ show st

stepFunCall ((Symbol "quote"):vs) st@State {stack=s} = return $ st {stack=vs++s}
stepFunCall ((Symbol "if"):[p,thenV,elseV]) st@State {work=ws, stack=s} = return $ st {work=(Eval p):If:ws, stack=thenV:elseV:s}
stepFunCall l@(f:fs) st@State {env = e, work=ws, stack=s} = if selfEvaluating f
                                      then return $ st {stack=SList l:s}
                                      else return $ st {work=evalListWork l++ws}
stepFunCall [] _ = fail "can't evaluate nil"

evalListWork :: [SNode] -> [Work]
evalListWork l = map (\a -> (Eval a)) l ++ [Apply l]

selfEvaluating :: SNode -> Bool
selfEvaluating s = s `elem` map Symbol ["lambda", "nlambda"]

primCall "car" [SList (a:gs)] = return a
primCall "cdr" [SList (a:gs)] = return $ SList gs
primCall "cons" [a,SList b] = return $ SList (a:b)
primCall "explode" [Symbol s] = return $ SList $ map (Symbol . return) s
primCall "implode" [SList symbols] = do strs <- mapM unsymbol symbols
                                        return $ Symbol $ concat strs
                        where unsymbol l@(SList _) = fail $ "called implode on a list " ++ show l
                              unsymbol (Symbol s) = return s
primCall "null" [v] = return $ if v == nil then true else false
primCall "atomp" [Symbol _] = return true
primCall "atomp" [_] = return false
primCall cmd args = fail $ "can't apply " ++ cmd ++ " to args: " ++ show args

run :: Monad m => State -> m SNode
run st = case run' st of
            Right ([val])  -> return val
            Right vals -> fail $ "Multiple values left on the stack: " ++ show vals
            Left err -> fail err
  
run' st = do v <- loop noMoreWork step (return st)
             return (stack v)

loop _ _ (Left e)  = Left e
loop p f (Right v) = if p v then Right v else loop p f (f v)

noMoreWork st = work st == []

lookup :: Monad m => String -> [(String,SNode)] -> m SNode
lookup s st = case P.lookup s st of
        Nothing -> fail $ "undefined reference to " ++ s
        (Just sym) -> return sym
