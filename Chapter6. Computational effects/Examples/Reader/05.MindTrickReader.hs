import Control.Monad
import Control.Monad.Reader

-- Think of any number.
-- Double the number.
-- Add 9 with result.
-- Subtract 3 with the result.
-- Divide the result by 2.
-- Subtract the number with the first number started with.
-- The answer will always be 3.


thinkOfANumber :: Integer -> Reader Integer Integer
thinkOfANumber = return

doubleTheNumber :: Integer -> Reader Integer Integer
doubleTheNumber = return . (2 *)


add9 :: Integer -> Reader Integer Integer
add9 = return . (9 +)

substract3 :: Integer -> Reader Integer Integer
substract3 x = return $ x -3

divideBy2 :: Integer -> Reader Integer Integer
divideBy2 x = return $ x `div` 2


substractTheFirstNumberYouStartedWith :: Integer -> Reader Integer Integer
substractTheFirstNumberYouStartedWith x = do
    theNumberYouStartedWith <- ask
    return $ x - theNumberYouStartedWith

mindTrick' :: Integer -> Reader Integer Integer
mindTrick' = thinkOfANumber >=> doubleTheNumber >=> add9 >=> substract3 >=> divideBy2 >=> substractTheFirstNumberYouStartedWith

mindTrick :: Integer -> Integer
mindTrick x = runReader (mindTrick' x) x