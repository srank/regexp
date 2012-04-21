module MatchTests(matchHereTestResults, matchTestResults) where

import Regexp
import MatchRegexp(matchHere, match)

matchHereTestResults :: 
  [(Regexp, String, [(String, String)], [(String, String)])]
matchHereTestResults = runTests matchHere matchHereTests

matchTestResults :: [(Regexp, String, [String], [String])]
matchTestResults = runTests match matchTests

matchHereTests :: [(Regexp, String, [(String, String)])]
matchHereTests = [(Literal "abc", "", []),
                  (Literal "x", "xbc", [("x", "bc")]),
                  (Or (Literal "x") (Literal "y"), "abc", []),
                  (Or (Literal "x") (Literal "y"), "xbc", 
                    [("x", "bc")]),
                  (OneOrMore (Literal "x"), "xxbc", 
                    [("x", "xbc"), ("xx", "bc")]),
                  (OneOrMore (Literal "x"), "yxx", []),
                  (ZeroOrMore (Literal "x"), "xxbc", 
                    [("", "xxbc"), ("x", "xbc"), ("xx", "bc")]),
                  (ZeroOrMore (Literal "x"), "yxx", [("", "yxx")]),
                  (Sequence (Literal "x") (Literal "z"), "xyz", []),
                  (Sequence (Literal "y") (Literal "z"), "xyz", []),
                  (Sequence (Literal "x") (Literal "y"), "xyz", [("xy", "z")]),
                  (Optional (Literal "x"), "xyz", [("x", "yz")]),
                  (Optional (Literal "x"), "yz", [("", "yz")]),
                  (AtEnd (Literal "x"), "xy", []),
                  (AtEnd (Literal "x"), "x", [("x", "")]),
                  (AnyChar, "xyz", [("x", "yz")])
                 ]

matchTests :: [(Regexp, String, [String])]
matchTests = [(AtStart (Literal "x"), "xyz", ["x"]),
              (ZeroOrMore AnyChar, "a", ["", "a"]),
              (ZeroOrMore AnyChar, "abc",
               ["", "a", "ab", "abc", "b", "bc", "c"]),
              (OneOrMore AnyChar, "abc",
               ["a", "ab", "abc", "b", "bc", "c"])
             ]

runTests :: (Show a, Eq a) => (Regexp -> String -> a) -> 
        [(Regexp, String, a)] -> 
        [(Regexp, String, a, a)]
runTests _ [] = []
runTests f ((r, t, expected):ss)
  | actual == expected = runTests f ss
  | otherwise = (r, t, actual, expected): runTests f ss
    where actual = f r t
