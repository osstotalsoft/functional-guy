import Data.List ( intersperse )
import System.Environment ( getArgs )

line :: Int -> Int -> [Char]
line total n = spaces ++ stars
  where
    spaces = replicate (total - n) ' '
    stars = intersperse ' ' $ replicate n '*'


tree :: Int -> [String]
tree n = map (line n) [1 .. n]

main :: IO ()
main = do
  [arg] <- getArgs
  let fn2 =  mapM_ putStrLn
  let fn1 = tree . read
  let fn = fn2 . tree . read
  mapM_  putStrLn . tree . read $ arg
  putStrLn "R7D 4ever!"

