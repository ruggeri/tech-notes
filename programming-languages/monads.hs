-- Some fooling around with a Writer monad

newtype Writer m a = Writer { runWriter :: (a, m) }

-- All Applicatives are Functors?
instance (Monoid m) => Functor (Writer m) where
  f `fmap` (Writer (a, v)) = Writer (f a, v)

-- All Monads are Applicatives? Why is that necessary?
instance (Monoid m) => Applicative (Writer m) where
  pure a = Writer (a, mempty)
  (Writer (f, v)) <*> (Writer (a, v')) =
    Writer (f a, v `mappend` v')

instance (Monoid m) => Monad (Writer m) where
  return x = pure x
  (Writer (x, v)) >>= f =
    let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

nextCollatz :: Integral t => t -> t
nextCollatz x
  | (x == 1) = 1
  | odd x = 3 * x + 1
  | even x = quot x 2

logNumber :: Integral t => t -> Writer [t] t
logNumber x = Writer (x, [x])

coupleCollatzSteps :: Integral t => t -> Writer [t] t
--coupleCollatzSteps x =
--  (nextCollatz' x) >>= (\x -> nextCollatz' x) >>= (\x -> nextCollatz' x)
coupleCollatzSteps val0 = do
  val1 <- nextCollatz' val0
  val2 <- nextCollatz' val1
  val3 <- nextCollatz' val2
  val4 <- nextCollatz' val3
  return val4
  where
    nextCollatz' x = Writer (nextCollatz x, [x])

collatzSequence' :: Integral t => Writer [t] t -> Writer [t] t
collatzSequence' w =
  w >>= \x ->
    if x == 1
    then pure x
    else collatzSequence' (Writer ((nextCollatz x), [x]))

collatzSequence :: Integral t => t -> Writer [t] t
collatzSequence x = collatzSequence' (pure x)

collatzSequence2 :: Integral t => t -> Writer [t] t
collatzSequence2 x
  | (x == 1)  = return x
  | otherwise = do
                  logNumber x
                  collatzSequence2 (nextCollatz x)

instance (Show m, Show a) => Show (Writer a m) where
  show (Writer (m, a)) = (show a) ++ " " ++ (show m)

-- Note that map print would not work here, as that produces [IO ()]
x :: IO ()
x = mapM_ print $ snd $ runWriter $ collatzSequence2 10

-- Some fooling around with a State Monad

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State g) = State (\s -> (let (a, s') = (g s) in (f a, s')))

instance Applicative (State s) where
  pure x = State (\s -> (x, s))
  (State f) <*> (State g) = State (\s ->
    let (x, s')   = (g s)
        (f', s'') = (f s')
    in (f' x, s''))

-- Do g first to the state.
-- Then do f to the result; which would decide how to advance the state.
-- Then actually advance the old state.
instance Monad (State s) where
  return = pure
  (State g) >>= f = State (\s ->
    let (a, s') = (g s)
        (State f') = (f a)
    in  (f' s'))

data Stack t = Empty | Cons t (Stack t)

instance (Show t) => Show (Stack t) where
  show Empty = "END"
  show (Cons x xs) = (show x) ++ "-" ++ (show xs)

pop :: State (Stack t) t
pop = State (\(Cons x xs) -> (x, xs))

push :: t -> State (Stack t) ()
push x = State (\xs -> ((), (Cons x xs)))

stackTest :: State (Stack Int) Int
stackTest = do
  push 3
  push 4
  push 5
  push 6
  x <- pop
  if x > 10
  then do
    push 7
    return x
  else
    pop

main :: IO ()
main = let results = (runState stackTest) Empty
       in print results
