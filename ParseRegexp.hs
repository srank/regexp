module ParseRegexp where
import Regexp

data Token = Text String |
             OpenBracket |
             CloseBracket |
             Star |
             Plus |
             QuestionMark |
             Either |
             Dot |
             Start |
             End
             deriving (Eq, Show)
             
tokenise :: String -> [Token]
tokenise [] = []
tokenise ('(':xs) = OpenBracket:tokenise xs
tokenise (')':xs) = CloseBracket:tokenise xs
tokenise ('*':xs) = Star:tokenise xs
tokenise ('+':xs) = Plus:tokenise xs
tokenise ('?':xs) = QuestionMark:tokenise xs
tokenise ('|':xs) = Either:tokenise xs
tokenise ('^':xs) = Start:tokenise xs
tokenise ('$':xs) = End:tokenise xs
tokenise ('.':xs) = Dot:tokenise xs
tokenise xs = Text f:tokenise g
  where (f, g) = span (\x -> not $ x `elem` specials) xs

specials = "()*+?|^$."

{- Regexp grammar:
TopRegexp: Regexp
         | '^' Regexp
         ;

Regexp: Literal
      | Dot
      | Regexp '|' Regexp
      | '(' Regexp ')'
      | Regexp '+'
      | Regexp '*'
      | Regexp '?'
      | Regexp '$' -- error if anything following
-}

parse :: [Token] -> Maybe Regexp
parse (Start:ts)
  | result == Nothing = error "Parse error on Start"
  | otherwise = Just $ AtStart (getRegexp result)
    where result = parse ts
          getRegexp (Just r) = r
                   
parse input 
  | null leftovers = result
  | otherwise = error $ "Leftover tokens: " ++ show leftovers
    where (result, leftovers) = matchRegexp Nothing input

matchRegexp :: Maybe Regexp -> [Token] -> (Maybe Regexp, [Token])
matchRegexp rs (Text t:ts) = 
  matchRegexp (sequenceIt rs $ Just (Literal t)) ts 
  
matchRegexp rs (Dot:ts) =
  matchRegexp (sequenceIt rs $ Just AnyChar) ts 
                             
matchRegexp rs (OpenBracket:ts)
  | r == CloseBracket = 
    matchRegexp (sequenceIt rs matched) remains
  | otherwise = error "mismatched brackets"
      where (matched, r:remains) = matchRegexp (Nothing) ts

matchRegexp (Just rs) (Plus:ts) = 
  matchRegexp (Just $ OneOrMore rs) ts

matchRegexp (Just rs) (Star:ts) = 
  matchRegexp (Just $ ZeroOrMore rs) ts

matchRegexp (Just rs) (End:ts)
  | null ts = (Just (AtEnd rs), [])
              | otherwise = error $ "After end: " ++ show ts

matchRegexp (Just rs) (QuestionMark:ts) = 
  (Just $ Optional rs, ts)

matchRegexp rs ts = (rs,ts)


sequenceIt :: Maybe Regexp -> Maybe Regexp -> Maybe Regexp
sequenceIt Nothing r = r
sequenceIt r Nothing = r
sequenceIt (Just r1) (Just r2) = Just $ Sequence r1 r2

matchIt regexp  =
  match $ getRegexp $ parse $ tokenise regexp
  where getRegexp (Just r) = r 
        getRegexp Nothing = error "Parse error"
