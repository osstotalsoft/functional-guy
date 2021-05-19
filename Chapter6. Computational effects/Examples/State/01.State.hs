{-# LANGUAGE TupleSections #-}

newtype State s a = State {runState :: s -> (s, a)}

instance Functor (State s) where
  fmap f k =
    State
      ( \s ->
          let (s', a) = runState k s
              b = f a
           in (s', b)
      )

instance Applicative (State s) where
  pure a = State (,a)
  sf <*> sa =
    State
      ( \s ->
          let (s', f) = runState sf s
              (s'', a) = runState sa s'
           in (s'', f a)
      )

instance Monad (State s) where
  sa >>= f =
    State
      ( \s ->
          let (s', a) = runState sa s
           in runState (f a) s'
      )
