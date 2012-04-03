module Grep (match) where


matchHere :: String -> String -> Maybe String
matchHere (r:rs) (x:xs)
 | rs /= [] && head rs == '*' = getStarMatchHere r (tail rs) (x:xs)
 | r `elem` [x, '.'] = appendMatch x result
 | otherwise = Nothing
   where result = matchHere rs xs
matchHere [] _ = Just []
matchHere _ [] = Nothing        

getStarMatchHere :: Char -> String -> String -> Maybe String
getStarMatchHere char restOfRegexp string@(x:xs)         
  | x /= char = Nothing
  | otherwise = (Just matchingChars)  `appendIfMatching` matchHere restOfRegexp unmatchedChars
                where matchingChars = takeWhile (==char) string
                      unmatchedChars = dropWhile (==char) string

appendIfMatching :: Maybe String -> Maybe String -> Maybe String
appendIfMatching Nothing _ = Nothing
appendIfMatching _ Nothing = Nothing
appendIfMatching (Just str1) (Just str2) = Just (str1 ++ str2)

-- Match the (restricted) regexp r:rs against the string x:xs
-- Magic characters so far: . ^
-- To do: | () ? * + [] \ $
-- To do even later: char classes, \W etc

match :: String -> String -> Maybe String
match regexp@(r:rs) text@(x:xs)
 | r == '^' = matchHere rs text
 | isMatch here = here
 | otherwise = match regexp xs                                  
   where here = matchHere regexp (x:xs)   
         isMatch (Just _) = True
         isMatch _ = False
match rs [] = Nothing                          

matchStar :: Char -> String -> String -> Maybe String
matchStar r rs (x:xs)
 | r == x = appendMatch r $ matchStarHere r rs xs
 | otherwise = Nothing


matchStarHere :: Char -> String -> String -> Maybe String
matchStarHere s rs (x:xs)
  | s == x = appendMaybe x $ matchStarHere s rs xs
  | otherwise = matchHere rs xs
matchStarHere s _ [] = Just ""
                 
appendMatch :: Char -> Maybe String -> Maybe String
appendMatch x Nothing = Nothing
appendMatch x (Just xs) = Just (x:xs)                 

appendMaybe :: Char -> Maybe String -> Maybe String
appendMaybe x Nothing = Just (x:[])
appendMaybe x (Just xs) = Just (x:xs)         
