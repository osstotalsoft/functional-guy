quicksort [] = []
quicksort (x : xs) =
  quicksort smallerThanX ++ [x] ++ quicksort biggerThanX
  where
    smallerThanX = filter (<= x) xs
    biggerThanX = filter (> x) xs

test = quicksort $ reverse [1 .. 10]