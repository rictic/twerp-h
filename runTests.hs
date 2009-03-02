{-# OPTIONS_GHC -fglasgow-exts #-}
module Main
  where

import Twerp
import TwerpParser
import Text.ParserCombinators.Parsec
import Control.Monad.Error

main = if all id tests
       then print "All clear"
       else print tests >> fail "A test failed"

tests = interpreterTests ++ (map parseB parserTests)

interpreterTests = [
    Twerp.lookup "x" [] == Nothing,
    Twerp.lookup "x" [("x", Symbol "good")] == Just (Symbol "good"),
    stack (stepR (stateToEval $ Symbol "car")) == [SList [Symbol "prim", Symbol "car"]],
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
      == [Eval (Symbol "cons"), Eval (Symbol "a"), Eval (Symbol "b"), Apply [Symbol "cons", Symbol "a", Symbol "b"]],
    runP "(car car)" == Symbol "prim",
    runP "(quote (car car))" == (SList $ map Symbol ["car", "car"]),
    runP "((lambda (a) (car a)) car)" == Symbol "prim",
    runErr "()"
   ]

stepR st = case step st of
        Left err -> error err
        Right val -> val
stepErr st = case step st of
        Nothing -> True
        Just val -> error $ "Expected an error, got: " ++ (show val)
runP :: String -> SNode
runP input = case parse snode "test" input of
     Left err → error (show err)
     Right s → case (run.stateToEval) s of
        Left err -> error err
        Right s -> s
runErr :: String -> Bool
runErr input = case parse snode "test" input of
     Left err -> True
     Right s -> case (run.stateToEval) s of
       Nothing -> True
       Just a -> False

parserTests :: [(String, SNode)]
parserTests = [("this", Symbol "this"),
               ("foo\\ bar", Symbol "foo bar"),
               ("(foo)", SList [Symbol "foo"]),
               ("(foo bar baz)", SList $ map Symbol ["foo", "bar", "baz"]),
               ("(foo (bar baz) bo)", SList [Symbol "foo", SList [Symbol "bar", Symbol "baz"], Symbol "bo"])]

parseB (input, target) = case parse snode "test" input of
    (Left err) → error (show err)
    (Right s) → if s == target then True else error ("expected " ++ (show target) ++ "\n but got " ++ (show s))
