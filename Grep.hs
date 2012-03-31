matchHere :: String -> String -> Bool
matchHere (r:rs) (x:xs)
 | r /= x = False
 | otherwise = matchHere rs xs
matchHere [] _ = True
matchHere (r:rs) [] = False

match :: String -> String -> Bool
match (r:rs) (x:xs)
 | r == x = matchHere rs xs
 | otherwise = match (r:rs) (xs)
match _ [] = False


getMatchHere :: String -> String -> Maybe String
getMatchHere (r:rs) (x:xs)
 | r == x || r == '.' = f x result
 | otherwise = Nothing
   where result = getMatchHere rs xs
         f :: Char -> Maybe String -> Maybe String
         f x Nothing = Nothing
         f x (Just xs) = Just (x:xs)
getMatchHere [] _ = Just []
getMatchHere _ [] = Nothing

getMatch :: String -> String -> Maybe String
getMatch (r:rs) (x:xs)
 | isMatch here = here
 | otherwise = getMatch (r:rs) xs                                  
                            where here = getMatchHere (r:rs) (x:xs)   
                                  isMatch (Just _) = True
                                  isMatch _ = False
   
testMatchHere = getMatch "xy" "123xy456" == Just "xy"
                && getMatch "x.y" "xzy" == Just "xzy"