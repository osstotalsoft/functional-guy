main :: IO ()
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]

-- Or we could do this:

main' :: IO ()
main' = do
    rs <- sequence [getLine, getLine, getLine]
    print rs