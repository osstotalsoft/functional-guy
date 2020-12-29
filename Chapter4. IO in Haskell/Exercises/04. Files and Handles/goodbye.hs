import System.IO

main :: IO ()
main = do
    helloFile <- openFile "hello.txt" ReadMode
    firstLine <- hGetLine helloFile -- get the first line from hello.txt file
    putStrLn firstLine 
    secondLine <- hGetLine helloFile -- get the second line from hello.txt file
    goodbyeFile <- openFile "goodbye.txt" WriteMode
    hPutStrLn goodbyeFile secondLine
    hClose helloFile
    hClose goodbyeFile
    putStrLn "done!"



