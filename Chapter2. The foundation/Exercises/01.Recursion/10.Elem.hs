elem' _ [] = False
elem' n (x : xs) = if n == x then True else elem' n xs

test = 10000 `elem'` [1 ..]