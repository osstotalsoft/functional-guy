readAnIntegerFromAFile :: IO Int
readAnIntegerFromAFile = return 2

getMessage :: Int -> String
getMessage int = "My computation resulted in: " ++ show int

newComputation :: IO ()
newComputation = do
 int <- readAnIntegerFromAFile -- we "bind" the result of readAnIntegerFromAFile to a name, 'int'
 putStrLn $ getMessage int -- 'int' holds a value of type Int
