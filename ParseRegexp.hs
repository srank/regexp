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
    
parseInsideBracket :: [Token] -> [Token] -> (Regexp, [Token])
parseInsideBracket soFar (t:ts)
  | t == CloseBracket = (parseMore soFar, ts)
--  | t == OpenBracket = 
  | otherwise = parseInsideBracket (soFar ++ [t]) ts
  

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