foldl' _ a [] = a
foldl' f a (x : xs) = foldl' f (f a x) xs

foldr' _ a [] = a
foldr' f a (x : xs) = f x (foldr' f a xs)

sum' = foldr' (+) 0

prod' = foldr' (*) 1

testSum = sum' [1 .. 100]

testProd = prod' [1 .. 10]