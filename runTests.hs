{-# OPTIONS_GHC -fglasgow-exts #-}
module Main
  where
import TwerpInterp
import TwerpParser
import Twerp
import Control.Monad.Error

--Short programs, and their results
twerpTests = 
    [("()", nil)
    ,("(car car)", Symbol "prim")
    ,("(quote (car car))", SList $ map Symbol ["car", "car"])
    ,("((lambda (a) (car a)) car)", Symbol "prim")
    ,("(explode 'abc)", SList [Symbol "a", Symbol "b", Symbol "c"])
    ,("(implode (explode 'abc))", Symbol "abc")
    ,("(if '#t 'a 'b)", Symbol "a")
    ,("(if '#f 'a 'b)", Symbol "b")
    ,("(if '() 'a 'b)", Symbol "b")
    ,("(null '())", true)
    ,("(null 'a)", false)
    ,("(null '(a))", false)
    ,("nil", nil)
    ,("#t", true)
    ,("#f", false)
    ,("(atomp 'a)", true)
    ,("(atomp '())", false)
    ,("(atomp '(a))", false)
    ,("(;\n)", nil)
    ,(";\n()", nil)
    ,("();", nil)
    ,("'(a;\nb)", SList [Symbol "a", Symbol "b"])
    ,("(call/cc (lambda (k) (k '42)))", Symbol "42")
    ,("(call/cc (lambda (k) (car (k '42))))", Symbol "42")
    ,("(eq 'a 'a)", true)
    ,("(eq 'a 'b)", false)
    ,("(eq 'a ())", false)
    ,("(eq () ())", false)
    ,("((lambda (a) (eval 'a)) 'foo)", Symbol "foo")
    ,("((lambda (a) (eval '(call/cc (lambda (k) (k '42))))) 'foo)", Symbol "42")
    ,("((lambda (a) (eval 'a)) (call/cc (lambda (k) (k '42)))))", Symbol "42")
    ]

--Trying to run these programs should give an error
twerpFailTests = []

--Programs and the values they should parse to
parserTests = [("foo", Symbol "foo"),
               ("foo\\ bar", Symbol "foo bar"),
               ("(foo)", SList [Symbol "foo"]),
               ("(foo bar baz)", SList $ map Symbol ["foo", "bar", "baz"]),
               ("(foo (bar baz) bo)", SList [Symbol "foo", SList [Symbol "bar", Symbol "baz"], Symbol "bo"]),
               ("'abc", SList [Symbol "quote", Symbol "abc"]),
               ("'(abc def)", SList [Symbol "quote", SList [Symbol "abc", Symbol "def"]])]


--Tests of the interpreter primitives
interpreterTests = [
    TwerpInterp.lookup "x" [] == Nothing,
    TwerpInterp.lookup "x" [("x", Symbol "good")] == Just (Symbol "good"),
    stack (step1 "car") == [SList [Symbol "prim", Symbol "car"]],
    (env $ stepR $ minSt {work=[Bind "x"], stack=[Symbol "good"]}) == [("x",Symbol "good")],
    (env $ stepR $ minSt {work=[Unbind "x"], env=[("x", Symbol "x")]}) == [],
    stepErr $ minSt {work=[Unbind "x"], env=[("bad", Symbol "x")]},
    (stack $ stepR $ minSt {work=[Apply [nil,nil]], stack=[SList [Symbol "a", Symbol "b"], SList [Symbol "prim", Symbol "car"]]})
     == [Symbol "a"],
    (stack $ stepR $ minSt {work=[Apply [nil,nil]], stack=[SList [Symbol "a", Symbol "b"], SList [Symbol "prim", Symbol "cdr"]]})
      == [SList [Symbol "b"]],
    (stack $ stepR $ minSt {work=[Apply [nil,nil,nil]], stack=[SList [Symbol "b"], Symbol "a", SList [Symbol "prim", Symbol "cons"]]})
      == [SList [Symbol "a", Symbol "b"]],
    (work $ stepR $ minSt {work=[Eval (SList [Symbol "cons", Symbol "a", Symbol "b"])]})
      == [Eval (Symbol "cons"), Eval (Symbol "a"), Eval (Symbol "b"), Apply [Symbol "cons", Symbol "a", Symbol "b"]]
   ]


runTwerpTest (v, c) = runP v == c || error ("error evaluating " ++ v ++ " - expected " ++ show c ++ "\n but got " ++ show (runP v))


tests = interpreterTests ++ map parseB parserTests ++ map runTwerpTest twerpTests ++ map runErr twerpFailTests


step1 input = case parse "test" input >>= (step . stateToEval) of
      Left err -> error err
      Right val -> val

stepR st = case step st of
        Left err -> error err
        Right val -> val
stepErr st = case step st of
        Nothing -> True
        Just val -> error $ "Expected an error, got: " ++ show val
runP :: String -> SNode
runP input = case parseRun input of
      Left err -> error err
      Right s -> s
runErr :: String -> Bool
runErr input = case parseRun input of
     Nothing -> True
     Just s -> error $ show s ++ " should have failed" 

parseRun input = parse "test" input >>= (run.stateToEval) 


parseB (input, target) = case parse "test" input of
    (Left err) -> error err
    (Right s) -> (s == target) || error ("expected " ++ show target ++ "\n but got " ++ show s)


main = if all id tests
       then putStrLn ((show . length) tests ++ " tests run, all passed")
       else print tests >> fail "A test failed"
