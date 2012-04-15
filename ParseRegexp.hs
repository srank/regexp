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

parseRegexp :: [Token] -> Regexp
parseRegexp input
  = parseTopRegexp input
    
parseTopRegexp :: [Token] -> Regexp
parseTopRegexp (Start:ts) = AtStart $ parseMore ts
parseTopRegexp ts
  = parseMore ts
    
parseMore :: [Token] -> Regexp    
parseMore ((Text string):[]) = Literal string
parseMore ((Text string):ts) = Sequence (Literal string) (parseMore ts)

parseMore (Dot:xs)
  | null xs = AnyChar
  | otherwise = Sequence AnyChar (parseMore xs)
                
parseMore (OpenBracket:tokens)
  | null remainder = regexp
  | otherwise = Sequence regexp (parseMore remainder)
    where (regexp, remainder) = parseInsideBracket [] tokens
          
parseMore x = error $ "oops " ++ show x          
    
parseInsideBracket :: [Token] -> [Token] -> (Regexp, [Token])
parseInsideBracket soFar (t:ts)
  | t == CloseBracket = (parseMore soFar, ts)
  | t == OpenBracket && null soFar = parseInsideBracket [] ts
  | t == OpenBracket = (Sequence (parseMore soFar) next, remains)
  | otherwise = parseInsideBracket (soFar ++ [t]) ts
    where (next, remains) = (parseInsideBracket [] ts)  


parseRe :: [Token] -> [Token] -> (Regexp, [Token])
parseRe [] (Text t:ts) = (Literal t, ts)
parseRe soFar (Text t:ts) = (Sequence previous (Literal t), ts)
  where (previous, remains) = parseRe [] soFar
parseRe soFar (OpenBracket:ts) = 
  parseInBracs [] ts 
  where
    parseInBracs :: [Token] -> [Token] -> (Regexp, [Token])
--    parseInBracs soFar (CloseBracket:[]) = parseRe soFar []
    parseInBracs soFar (CloseBracket:ts) = (reg, ts)
      where (reg, remainder) = parseRe [] soFar
    parseInBracs soFar (OpenBracket:ts) = 
      (regexp, remains)
        where (regexp, remains) = parseRe [] soFar
    parseInBracs soFar (t:ts) = parseInBracs (soFar++[t]) ts

parseRe soFar ts = error $ "> " ++ show soFar ++ ", " ++ show ts

tryout = [OpenBracket, OpenBracket, Text "a", 
                     CloseBracket, CloseBracket, Text "b"]
simple = [OpenBracket, Text "a", CloseBracket, Text "b"]

{-getNextRe :: [Token] -> ([Token], [Token])
getNextRe (Text t:ts) = ([Text t], ts)
getNextRe (OpenBracket:ts)
 = getNextBrac [] ts
 -}
 
getNextBrac :: Regexp -> [Token] -> (Regexp, [Token])   
getNextBrac soFar (CloseBracket:ts) = (soFar, ts)
getNextBrac soFar (Text t:ts) = 
  getNextBrac (Sequence soFar (Literal t)) ts
  

matchBrac :: Regexp -> [Token] -> (Regexp, [Token])
matchBrac soFar (CloseBracket:ts) = (soFar, ts)
matchBrac soFar (OpenBracket:ts) = 
  Sequence matchBrac soFar 
matchBrac soFar (Text t:ts) = matchBrac (Sequence soFar (Literal t)) ts

-- attempt to be more smart
{-
specialChars :: Map Char Token
specialChars = fromList [('(', OpenBracket), 
                (')', CloseBracket),
                ('*', Star),
                ('+', Plus),
                ('?', QuestionMark),
                ('|', Either),
                ('^', Start),
                ('$', End)]
               
tokenise' :: String -> [Token]
tokenise' [] = []
tokenise' (x:xs)
  | value == Nothing = (Text (x:[])):tokenise' xs
  | otherwise = getValue value:tokenise' xs
    where value = Data.Map.lookup x specialChars
          getValue (Just x) = x

-}