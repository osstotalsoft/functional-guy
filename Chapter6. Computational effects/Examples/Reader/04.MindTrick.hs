-- Think of any number.
-- Double the number.
-- Add 9 with result.
-- Subtract 3 with the result.
-- Divide the result by 2.
-- Subtract the number with the first number started with.
-- The answer will always be 3.

mindTrick :: Integer -> Integer
mindTrick x = (x * 2 + 9 -3) `div` 2 - x

-- rewrite it as a pipeline of functions
thinkOfANumber :: Integer -> Integer
thinkOfANumber = id

doubleTheNumber :: Integer -> Integer
doubleTheNumber = (2 *)

add9 :: Integer -> Integer
add9 = (9 +)

substract3 :: Integer -> Integer
substract3 x = x -3

divideBy2 :: Integer -> Integer
divideBy2 x = x `div` 2

substractTheFirstNumberYouStartedWith :: Integer -> Integer -> Integer
substractTheFirstNumberYouStartedWith x theNumberYouStartedWith = x - theNumberYouStartedWith

(>>>) :: (a -> b) -> (b -> c) -> a -> c
f >>> g = g . f

mindTrick' :: Integer -> Integer -> Integer
mindTrick' = thinkOfANumber >>> doubleTheNumber >>> add9 >>> substract3 >>> divideBy2 >>> substractTheFirstNumberYouStartedWith

mindTrick'' :: Integer -> Integer
mindTrick'' x = thinkOfANumber >>> doubleTheNumber >>> add9 >>> substract3 >>> divideBy2 >>> (`substractTheFirstNumberYouStartedWith` x) $ x
