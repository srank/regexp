module Grep (match) where


matchHere :: String -> String -> Maybe String
matchHere (r:rs) (x:xs)
 | r `elem` [x, '.'] = appendMatch x result
 | otherwise = Nothing
   where result = matchHere rs xs
         appendMatch :: Char -> Maybe String -> Maybe String
         appendMatch x Nothing = Nothing
         appendMatch x (Just xs) = Just (x:xs)
matchHere [] _ = Just []
matchHere _ [] = Nothing


-- Match the (restricted) regexp r:rs against the string x:xs
-- Magic characters so far: . ^

match :: String -> String -> Maybe String
match regexp@(r:rs) text@(x:xs)
 | r == '^' = matchHere rs text
 | isMatch here = here
 | otherwise = match regexp xs                                  
   where here = matchHere regexp (x:xs)   
         isMatch (Just _) = True
         isMatch _ = False
match rs [] = Nothing                          
