maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)
