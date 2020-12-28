main :: IO ()
main = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line


main' :: IO ()
main' = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b