import Data.Char (toUpper)

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()

main' :: IO ()
main' =
  putStrLn "Hello! What's your name?"
    >> getLine
    >>= ( \firstName ->
            putStrLn "What's your last name?"
              >> getLine
              >>= ( \lastName ->
                      let bigFirstName = map toUpper firstName
                          bigLastName = map toUpper lastName
                       in putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
                  )
        )


main = do
  putStrLn "What's your first name?"
  firstName <- getLine

  putStrLn "What's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"