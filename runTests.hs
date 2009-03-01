{-# OPTIONS_GHC -fglasgow-exts #-}
module Main
  where

import Twerp
import TwerpParser
import Text.ParserCombinators.Parsec

main = if all id tests
       then print "All clear"
       else print tests >> fail "A test failed"

tests = interpreterTests ++ (map parseB parserTests)

interpreterTests = [
    Twerp.lookup "x" [] == nil,
    Twerp.lookup "x" [("x", Symbol "good")] == Symbol "good",
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
      == [Eval (Symbol "cons"), Eval (Symbol "a"), Eval (Symbol "b"), Apply [Symbol "cons", Symbol "a", Symbol "b"]],
    runP "(car car)" == Symbol "prim"
   ]

runP :: String -> SNode
runP input = case parse snode "test" input of
     (Left err) → error (show err)
     (Right s) → run $ stateToEval s


parserTests :: [(String, SNode)]
parserTests = [("this", Symbol "this"),
               ("foo\\ bar", Symbol "foo bar"),
               ("(foo)", SList [Symbol "foo"]),
               ("(foo bar baz)", SList $ map Symbol ["foo", "bar", "baz"]),
               ("(foo (bar baz) bo)", SList [Symbol "foo", SList [Symbol "bar", Symbol "baz"], Symbol "bo"])]

parseB (input, target) = case parse snode "test" input of
    (Left err) → error (show err)
    (Right s) → if s == target then True else error ("expected " ++ (show target) ++ "\n but got " ++ (show s))