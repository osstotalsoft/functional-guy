



main :: IO ()
main =  do
        a <- getLine
        b <- getLine
        c <- getLine
        print [a,b,c]


-- Or we could do this:


list :: [Char]
list = ['H','e','l','l','o']


iolist :: [IO ()]
iolist =  map putChar list


main'' = sequence iolist


main' :: IO ()
main' = do
    rs <- sequence [getLine, getLine, return "5"]
    print rs



    