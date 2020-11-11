cycle' (x : xs) = x : cycle' (xs ++ [x])

test = take 5 . cycle' $ [1, 2]