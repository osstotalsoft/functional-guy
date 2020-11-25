--myAverage aList =  sum aList / length aList
myAverage aList = sum aList / fromIntegral (length aList)
myAverage' aList = sum aList `div` length aList


half n = n / 2
half' n = fromIntegral n / 2
half'' = (`div` 2)


y = 2 :: Int
x = toInteger y