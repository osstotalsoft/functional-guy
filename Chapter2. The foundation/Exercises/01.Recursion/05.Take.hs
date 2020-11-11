take' 0 _ = []
take' _ [] = []
take' n (x : xs) = x : take' (n -1) xs