> match :: String -> String -> Bool
> match (r:rs) (t:ts)
>  | r == '^' = matchHere rs (t:ts)
>  | matchHere (r:rs) (t:ts) = True
>  | otherwise = False -- fixme

> matchHere :: String -> String -> Bool
> matchHere [] _ = True
> matchHere (r:rs) text
>  | r == '*' = matchStar (head rs) (tail rs) text
>  | r == '$' = rs == [] && text == []
>  | text /= [] && (r == '.' || r == (head text))
>     = matchHere rs (tail text)

> matchStar :: Char -> String -> String -> Bool
> matchStar c regexp text
>  | matchHere regexp text = True
>  | otherwise = False -- fixme
