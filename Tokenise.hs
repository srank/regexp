module Tokenise(Token(Text,
             OpenBracket,
             CloseBracket,
             Star,
             Plus,
             QuestionMark,
             Either,
             Dot,
             Start,
             End), tokenise) where

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
  where (f, g) = getFrontLiteral xs
        
getFrontLiteral :: String -> (String, String)
getFrontLiteral = frontLiteral [] 

frontLiteral :: String -> String -> (String, String)
frontLiteral acc [] = (acc, [])
frontLiteral acc (t:ts)
      | t `elem` specialChars = (acc, t:ts)
      | t == '\\' = frontLiteral (acc ++ [head ts]) $ tail ts
      | otherwise = frontLiteral (acc ++ [t]) ts

specialChars :: [Char]
specialChars = "()*+?|^$."
                      