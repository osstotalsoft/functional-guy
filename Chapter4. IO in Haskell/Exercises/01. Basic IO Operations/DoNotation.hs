helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"


main :: IO ()
main = putStrLn "Hello! What's your name?" >>
            getLine >>= 
            (\name -> 
                return (helloPerson name)) >>=
            putStrLn


-- main' :: IO ()
-- main' = putStrLn "Hello! What's your name?" >>
--             getLine >>= 
--             (return . helloPerson) >>=
--             putStrLn
