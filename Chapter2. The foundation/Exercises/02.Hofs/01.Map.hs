incAll' [] = []
incAll' (x : xs) = x + 1 : incAll' xs

doubleAll' [] = []
doubleAll' (x : xs) = x * 2 : doubleAll' xs

map' _ [] = []
map' f (x : xs) = f x : map' f xs

incAll = map (+ 1)

doubleAll = map (* 2)