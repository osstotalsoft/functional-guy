todoList :: [IO ()]
todoList = [putChar 'a',
            do putChar 'b'
               putChar 'c',
            do c <- getChar
               putChar c]


sequence_' :: Monad m => [m ()] -> m [()]
sequence_' [] =  return [()]
sequence_'(a:as) = do a 
                      sequence as


putStr' :: [Char] -> IO ()
putStr' s =  sequence_ (map putChar s)