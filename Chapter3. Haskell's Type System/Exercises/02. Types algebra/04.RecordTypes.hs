data Cube = Cube
  { width :: Int,
    height :: Int,
    depth :: Int
  }

cube1 = Cube {width = 1, height = 1, depth = 1}

cube2 = Cube 1 1 1

cube3 = cube2 {depth = 2}

w1 = width cube1

w2 = width cube2


show'' :: Cube -> String
show'' cube = show (width cube) ++ " " ++ show (height cube) ++ " " ++ show (depth cube)