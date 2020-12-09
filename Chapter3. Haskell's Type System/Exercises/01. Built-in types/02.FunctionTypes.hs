--function types
myFunc :: Integer -> Integer
myFunc n = n * 2

makeCar :: String -> String -> Double -> (String, String, Double)
makeCar make model cc = (make, model, cc)

-- currying and partial application
myCar :: (String, String, Double)
myCar = makeCar "BMW" "X5" 3.0

myBmw :: String -> Double -> (String, String, Double)
myBmw = makeCar "BMW"

myX5 :: Double -> (String, String, Double)
myX5 = myBmw "X5"

myX5_30 :: (String, String, Double)
myX5_30 = myX5 3.0


--hof with function param
ifEven :: (Int -> Int) -> Int -> Int
ifEven f n =
  if even n
    then f n
    else n