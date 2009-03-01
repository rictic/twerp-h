{-# OPTIONS_GHC -fglasgow-exts #-}
module Main
  where
import Prelude hiding (lookup)
import qualified Prelude as P

type Primtype = State -> SNode -> (SNode,State)
data SNode = SList [SNode] | Symbol String deriving Eq
data State = State { env :: [(String, SNode)], 
                       work :: [Work], 
                       stack :: [SNode], 
                       intern :: [(String, SNode)] } 
            | ErrState String deriving Show
data Work = Eval SNode | Bind String | Unbind String | Apply [SNode] deriving (Show, Eq)
--type statefunc a = State -> (State, a)

instance Show SNode where
  show (Symbol s) = s
  show (SList l) = show l

nil = SList []
prim a = (a, SList [Symbol "prim", Symbol a])
builtins = map prim ["car", "cdr", "cons"]

minSt = State {env = [], work = [], stack = [], intern = builtins}
stateToEval :: SNode -> State
stateToEval initialExpr = minSt {work = [Eval initialExpr]}

step :: State -> State
step State {env = e, work = (Eval (Symbol x)):ws, stack=s, intern=i} = State {env=e, work=ws, stack=lookup x (e++i):s, intern=i}
step State {env = e, work = (Bind x):ws, stack=val:s, intern=i} = State {env=(x,val):e, work=ws, stack=s,intern=i}
step State {env = (k,v):e, work = (Unbind x):ws, stack=s, intern=i} = if k == x then State {env=e, work=ws, stack=s,intern=i}
                                                                              else ErrState ("Expected unbind " ++ x ++ " found " ++ k)
step State {env = e, work = (Apply argC):ws, stack=s, intern=i} = State {env=e, work=ws, stack=result:stk,intern=i}
                              where (args,stk) = splitAt (length argC) s
                                    result = applyPrimOrLambda (last args) (reverse $ init args)

step State {env = e, work = (Eval (SList l)):ws, stack=s, intern=i} = 
                                      State {env=e, work=wl++ws, stack=s, intern=i}
                                    where wl = evalListWork l

evalListWork :: [SNode] -> [Work]
evalListWork l = (map (\a -> (Eval a)) l) ++ [(Apply l)]

applyPrimOrLambda (SList ((Symbol "prim"):[Symbol p])) args = primCall p args


primCall "car" [SList (a:gs)] = a
primCall "cdr" [SList (a:gs)] = SList gs
primCall "cons" [a,SList b] = SList (a:b)


lookup :: String -> [(String,SNode)] -> SNode
lookup s st = case P.lookup s st of
        Nothing -> nil
        (Just sym) -> sym


main = if all id tests
       then print "All clear"
       else print tests >> fail "A test failed"

tests :: [Bool]
tests = [lookup "x" [] == nil,
         lookup "x" [("x", Symbol "good")] == Symbol "good",
         stack (step (stateToEval $ Symbol "car")) == [SList [Symbol "prim", Symbol "car"]],
         (env $ step $ minSt {work=[Bind "x"], stack=[Symbol "good"]}) == [("x",Symbol "good")],
         (env $ step $ minSt {work=[Unbind "x"], env=[("x", Symbol "x")]}) == [],
         isErrState $ step $ minSt {work=[Unbind "x"], env=[("bad", Symbol "x")]},
         (stack $ step $ minSt {work=[Apply [nil,nil]], stack=[SList [Symbol "a", Symbol "b"], SList [Symbol "prim", Symbol "car"]]})
          == [Symbol "a"],
         (stack $ step $ minSt {work=[Apply [nil,nil]], stack=[SList [Symbol "a", Symbol "b"], SList [Symbol "prim", Symbol "cdr"]]})
           == [SList [Symbol "b"]],
         (stack $ step $ minSt {work=[Apply [nil,nil,nil]], stack=[SList [Symbol "b"], Symbol "a", SList [Symbol "prim", Symbol "cons"]]})
           == [SList [Symbol "a", Symbol "b"]],
         (work $ step $ minSt {work=[Eval (SList [Symbol "cons", Symbol "a", Symbol "b"])]})
           == [Eval (Symbol "cons"), Eval (Symbol "a"), Eval (Symbol "b"), Apply [Symbol "cons", Symbol "a", Symbol "b"]]
        ]
        
isErrState (ErrState s) = True
isErrState _ = False