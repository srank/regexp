module Grep (match) where


matchHere :: String -> String -> Maybe String
matchHere (r:rs) (x:xs)
 | rs /= [] && head rs == '+' = getPlusMatchHere r (tail rs) (x:xs)
 | rs /= [] && head rs == '?' = getOptionalMatchHere r (tail rs) (x:xs)
 | r `elem` [x, '.'] = appendMatch x result
 | otherwise = Nothing
   where result = matchHere rs xs
matchHere [] _ = Just []
matchHere _ [] = Nothing        

getOptionalMatchHere :: Char -> String -> String -> Maybe String
getOptionalMatchHere r restOfRegexp (t:text)
 | r == t = appendMaybe r $ matchHere restOfRegexp text
 | otherwise = matchHere restOfRegexp (t:text)
 

getPlusMatchHere :: Char -> String -> String -> Maybe String
getPlusMatchHere char restOfRegexp string@(x:xs)         
  | x /= char = Nothing
  | otherwise = (Just matchingChars)  `appendIfMatching` matchHere restOfRegexp unmatchedChars
                where matchingChars = takeWhile (==char) string
                      unmatchedChars = dropWhile (==char) string

appendIfMatching :: Maybe String -> Maybe String -> Maybe String
appendIfMatching Nothing _ = Nothing
appendIfMatching _ Nothing = Nothing
appendIfMatching (Just str1) (Just str2) = Just (str1 ++ str2)

-- Match the (restricted) regexp r:rs against the string x:xs
-- Magic characters so far: . ^ +
-- To do: | ? + \ $ *
-- To do even later: [], (), char classes, \W etc

match :: String -> String -> Maybe String
match regexp@(r:rs) text@(x:xs)
 | r == '^' = matchHere rs text
 | isMatch here = here
 | otherwise = match regexp xs                                  
   where here = matchHere regexp (x:xs)   
         isMatch (Just _) = True
         isMatch _ = False
match rs [] = Nothing                          



appendMatch :: Char -> Maybe String -> Maybe String
appendMatch x Nothing = Nothing
appendMatch x (Just xs) = Just (x:xs)                 

appendMaybe :: Char -> Maybe String -> Maybe String
appendMaybe x Nothing = Just (x:[])
appendMaybe x (Just xs) = Just (x:xs)         
