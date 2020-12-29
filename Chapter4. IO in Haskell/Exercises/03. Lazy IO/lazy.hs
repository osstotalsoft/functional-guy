-- import System.Environment
-- import Control.Monad
-- main :: IO ()
-- main = do
--     args <- getArgs
--     let linesToRead = if length args > 0
--         then read (head args)
--         else 0 :: Int
--     numbers <- replicateM linesToRead getLine
--     let ints = map read numbers :: [Int]
--     print (sum ints)


main :: IO ()
main = do
    userInput <- getContents
    print userInput -- press Crts+Z to close the stream

-- main' :: IO ()
-- main' = do
--     userInput <- getContents
--     mapM_ print userInput

