sum' [] = 0
sum' (x : xs) = x + sum' xs

prod' [] = 1
prod' (x : xs) = x * prod' xs

--sum' = foldr' (+) 0
--prod' = foldr' (*) 1

foldr' _ a [] = a
foldr' f a (x : xs) = f x (foldr' f a xs)

anyTrue = foldr' (||) False

alltrue = foldr' (&&) False

-- [1, 2, 3] = 1:2:3:[]
-- foldr (+) 0 [1, 2, 3] = 1+2+3+0

copy = foldr' (:) []

append a b = foldr' (:) b a

length = foldr' count 0
  where
    count _ n = n + 1

doubleall = foldr' doubleandcons []
  where
    doubleandcons n list = (2 * n) : list

doubleandcons = fandcons double
  where
    double n = 2 * n
    fandcons f el list = f el : list

--fandcons f = (:) . f

doubleall' = foldr' ((:) . double) []
  where
    double n = 2 * n

--doubleall' = map double
map' f = foldr' ((:) . f) []
