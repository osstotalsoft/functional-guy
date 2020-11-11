filter' _ [] = []
filter' f (x : xs) = if f x then x : filter' f xs else xs

filterEven = filter' even

filterGt0 = filter' (> 0)