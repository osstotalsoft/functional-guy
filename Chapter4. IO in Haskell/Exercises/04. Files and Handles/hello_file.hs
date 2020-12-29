import System.IO

-- openFile :: FilePath -> IOMode -> IO Handle

main :: IO ()
main = do
    myFile <- openFile "hello.txt" ReadMode
    hClose myFile
    putStrLn "done!"