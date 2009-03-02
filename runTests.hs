{-# OPTIONS_GHC -fglasgow-exts #-}
module Main
  where
import TwerpInterp
import TwerpParser
import Control.Monad.Error

main = if all id tests
       then putStrLn (((show . length) tests) ++ " tests run, all passed")
       else print tests >> fail "A test failed"

tests = interpreterTests ++ (map parseB parserTests) ++ (map runTwerpTest twerpTests) ++ (map runErr twerpFailTests)

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

step1 input = case parse "test" input >>= (step . stateToEval) of
      Left err -> error err
      Right val -> val

stepR st = case step st of
        Left err -> error err
        Right val -> val
stepErr st = case step st of
        Nothing -> True
        Just val -> error $ "Expected an error, got: " ++ (show val)
runP :: String -> SNode
runP input = case parseRun input of
      Left err -> error err
      Right s -> s
runErr :: String -> Bool
runErr input = case parseRun input of
     Nothing -> True
     Just s -> error $ (show s) ++ " should have failed" 

parseRun input = parse "test" input >>= (run.stateToEval) 

parserTests :: [(String, SNode)]
parserTests = [("this", Symbol "this"),
               ("foo\\ bar", Symbol "foo bar"),
               ("(foo)", SList [Symbol "foo"]),
               ("(foo bar baz)", SList $ map Symbol ["foo", "bar", "baz"]),
               ("(foo (bar baz) bo)", SList [Symbol "foo", SList [Symbol "bar", Symbol "baz"], Symbol "bo"]),
               ("'(abc)", SList [Symbol "quote", Symbol "abc"])]

parseB (input, target) = case parse "test" input of
    (Left err) → error err
    (Right s) → if s == target then True else error ("expected " ++ (show target) ++ "\n but got " ++ (show s))

runTwerpTest (v, c) = if (runP v) == c then True else error ("expected " ++ (show c) ++ "\n but got " ++ (show (runP v)))

twerpTests = [("(car car)", Symbol "prim"),
    ("(quote (car car))", SList $ map Symbol ["car", "car"]),
    ("((lambda (a) (car a)) car)", Symbol "prim"),
    ("(explode '(abc))", SList [Symbol "a", Symbol "b", Symbol "c"])]

twerpFailTests = ["()"]