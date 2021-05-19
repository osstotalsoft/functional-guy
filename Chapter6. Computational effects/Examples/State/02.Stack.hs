import Control.Monad.State.Lazy

type MyState = Int

isEven :: Integer -> Bool
isEven = even

statefullIsEven :: Integer -> MyState -> (Bool, MyState)
statefullIsEven x s = (even x, s + 1)

statefullToString :: Bool -> MyState -> (String, MyState)
statefullToString x s = (show x, s + 2)

-- statefullComposition = statefullToString . statefullIsEven

--type State s a = s -> (a,s)
--newtype State s a = State {runState::s -> (a,s)}

statefullIsEven' :: Integer -> State MyState Bool
statefullIsEven' x = do
  -- s <- get
  -- put $ s + 1
  modify (+ 1)
  return $ even x

statefullToString' :: Bool -> State MyState String
statefullToString' x = do
  modify (+ 2)
  return $ show x

statefullComposition' :: Integer -> State MyState String
statefullComposition' = statefullIsEven' >=> statefullToString'

-------------------------------------------------------------------------------
type Stack a = [a] -- o structura de date care fct dupa principiul LIFO

--pop:: Stack a -> a
pop :: State (Stack a) a
pop = do
  -- s <- get
  -- let (x : xs) = s
  -- put xs
  -- return x
  h <- gets head
  modify tail
  return h

popAll :: State (Stack a) [a]
popAll = do
  l <- gets length
  replicateM l pop

-- if l == 0
--   then return []
--   else -- do
--   --   x <- pop
--   --   xs <- popAll
--   --   return $ x : xs
--     liftM2 (:) pop popAll

push :: a -> State (Stack a) ()
push = modify . (:)

pushAll :: [a] -> State (Stack a) ()
pushAll = mapM_ push

myStupidStatefullAction :: State (Stack Int) Int
myStupidStatefullAction = do
  push 1
  push 2
  pop

reverse' :: [a] -> State (Stack a) [a]
reverse' xs = do
  pushAll xs
  popAll

reverse'' :: [a] -> [a]
reverse'' xs = evalState (reverse' xs) []
