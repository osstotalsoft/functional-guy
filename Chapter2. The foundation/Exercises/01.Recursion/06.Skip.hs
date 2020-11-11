skip' 0 xs = xs
skip' _ [] = []
skip' n (_ : xs) = skip' (n -1) xs