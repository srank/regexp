module ParseRegexp(parseRegexp, parse) where

import Regexp
import Tokenise
                      
parseRegexp :: String -> Maybe Regexp                      
parseRegexp = parse . tokenise

parse :: [Token] -> Maybe Regexp
parse (Start:ts)
  | result == Nothing = error "Parse error on Start"
  | otherwise = Just $ AtStart (getRegexp result)
    where result = parse ts
          getRegexp (Just r) = r
          getRegexp Nothing = error "no regexp defined"
                   
parse input 
  | null leftovers = result
  | otherwise = error $ "Leftover tokens: " ++ show leftovers
    where (result, leftovers) = buildRegexp Nothing input

buildRegexp :: Maybe Regexp -> [Token] -> (Maybe Regexp, [Token])
buildRegexp rs (Text t:ts) = 
  buildRegexp (sequenceIt rs $ Just (Literal t)) ts 
  
buildRegexp rs (Dot:ts) =
  buildRegexp (sequenceIt rs $ Just AnyChar) ts 
                             
buildRegexp rs (OpenBracket:ts)
  | r == CloseBracket = 
    buildRegexp (sequenceIt rs matched) remains
  | otherwise = error "mismatched brackets"
      where (matched, r:remains) = buildRegexp Nothing ts

buildRegexp (Just rs) (Plus:ts) = 
  buildRegexp (Just $ OneOrMore rs) ts

buildRegexp (Just rs) (Star:ts) = 
  buildRegexp (Just $ ZeroOrMore rs) ts

buildRegexp (Just rs) (End:ts)
  | null ts = (Just (AtEnd rs), [])
  | otherwise = error $ "After end: " ++ show ts

buildRegexp (Just rs) (QuestionMark:ts) = 
  (Just $ Optional rs, ts)

buildRegexp rs ts = (rs, ts)


-- This is almost monadic
sequenceIt :: Maybe Regexp -> Maybe Regexp -> Maybe Regexp
sequenceIt Nothing r = r
sequenceIt r Nothing = r
sequenceIt (Just r1) (Just r2) = Just $ Sequence r1 r2

