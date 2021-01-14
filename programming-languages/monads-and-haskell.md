
(To run these programs, you can `stack runghc script.hs`).

Haskell uses monads for IO. Basically, it builds up an object tha
represents the transactions that should take place throughout the
program.

Do notation is just a shorthand to make this simpler.

I see this as kind of like CPS, actually.

## `Data.Functor`

Note some of this is defined in `Data.Functor`, while some is defined in
`GHC.Base`.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Basically, `fmap` generalizes the concept of `map`. You can apply it to
a 'boxed' thing. The simplest example is `Data.Maybe`:

```haskell
x = fmap (show :: Int -> String) (Just 3)
y = fmap (show :: Int -> String) (Nothing)

main = do
  -- Prints 'Just "3"'
  putStrLn (show x)
  -- Prints 'Nothing'
  putStrLn (show y)
```

They also define the helper `<$>`, which is an infix version of `fmap`.
Note that this is an analogue to the `$` command which is usually just
syntax to help eliminate parentheses.

```haskell
-- my definition of <$>
-- f <$> obj = fmap f obj

x = (show :: Int -> String) <$> (Just 3)
y = (show :: Int -> String) <$> (Nothing)

main = do
  -- Prints 'Just "3"'
  putStrLn (show x)
  -- Prints 'Nothing'
  putStrLn (show y)
```

Haskell says: "Whereas `Prelude.$` is function application, `<$>` is
function application *lifted* over a `Functor`." Here we see the meaning
of `lift`: it means something like: doing something through a kind of
box.

`Data.List` also is a `Functor`.

```haskell
-- From GHC.Base
instance Functor [] where
    fmap = map

xs = (show :: Int -> String) <$> [1, 2, 3]

main = do
  -- Prints ["1", "2", "3"]
  putStrLn (show xs)
```

Another example is `IO`:

```haskell
-- From GHC.Base
-- `pure . f` says: first take the incoming value, transform it with
-- `f`. Now, we need to return an IO instance, so wrap it via pure.
instance Functor IO where
   fmap f x = x >>= (pure . f)

-- Here I write the same thing myself in do notation.
myFmap :: (a -> b) -> IO a -> IO b
myFmap f action = do
  x <- action
  return (f x)

readStringAndAppendMsg = fmap (++ " is the input string") getLine

-- Maybe easiest to read with <&>, the flipped version of <$>.
-- Just import (<&>) the proper name of the infix operator <&>.
import Data.Functor ((<&>))
readStringAndAppendMsg = getLine <&> (++ " is the input string")

main = do
  putStrLn "Please type input string"
  s <- readStringAndAppendMsg
  putStrLn s
```

* Sources:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Functor.html
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Base.html#Functor
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.Functor.html

## `Control.Applicative` Functor

We say *Applicative Functor*, but the Haskell typeclass is named
`Applicative`. An `Applicative` extends the concept of a `Functor` by
allowing that the `Functor` might wrap a function, which in turn can be
applied.

```haskell
-- From GHC.Base
class Functor f => Applicative f where
    -- Lifts a value into the Applicative.
    pure :: a -> f a
    -- Takes a function wrapped in an Applicative and applies it to
    -- another applicative.
    (<*>) :: f (a -> b) -> f a -> f b
```

Here's an example with `IO`:

```haskell
import Data.Functor ((<&>))

-- Define functions that double/triple
double x = 2 * x
triple x = 3 * x

-- Let user select which function to use.
selectDoubleOrTriple :: IO (Int -> Int)
selectDoubleOrTriple = do
  putStrLn "Choose to double or triple a number"
  option <- getLine
  return (
    if (option == "double") then double else triple
    )

-- Notice the use of `<*>`. Also notice the use of `pure` to lift the
-- value 3.
doubleOrTripleThree :: IO Int
doubleOrTripleThree = selectDoubleOrTriple <*> (pure 3)

-- Last, notice the use of `<&>`. This is simply the flip of `<$>` and
-- thus lifting the second argument of &.
main :: IO ()
main = (doubleOrTripleThree <&> show) >>= putStrLn
```

The implementation of the typeclass `Applicative` for `IO` is a little
too much for me to understand right now (especially because I don't
understand IO/do notation yet)!

A perhaps simpler example with the `Maybe` `Applicative`:

```
x = (Just show) <*> (Just 3)
y = (Nothing :: (Maybe (Int -> String))) <*> (Just 3)
z = (Just (show :: Int -> String)) <*> Nothing

main = do
  putStrLn (show x)
  putStrLn (show y)
  putStrLn (show z)
```

Here we see the implementation:

```haskell
-- From GHC.Base
instance Applicative Maybe where
    -- Simply wrap the value.
    pure = Just

    -- To use <*>, unwrap `f` if any, and then delegate to `fmap`.
    Just f  <*> m       = fmap f m
    Nothing <*> _m      = Nothing
```

There is a synonym of `<*>`, `liftA2`:

```haskell
-- This is defined in the typeclass `Applicative` in case you have a
-- more efficient specialization.
liftA2 :: (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = (fmap f x) <*> y

--They contrast this with `liftA`, defined in `GHC.Base`:
liftA :: (a -> b) -> f a -> f b
liftA f x = f <$> x
```

The Learn You A Haskell folks say that the difference between Functor
and Applicative is made clear by `liftA` vs `liftA2`. They note that
only an applicative could have `liftA2` defined, since `liftA2 f x` can
produce an intermediate result of type `Applicative (b -> c)`.

Last, let's look at something which is a little weird. Let's consider
how to implement `Applicative` for `[]`.

```haskell
instance Applicative [] where
  -- Duh.
  pure x = [x]

  -- Notice that this doesn't work element-wise. In particular, we
  -- expect of all `Applicative`s that `(pure id) <*> v == v`.
  fs <*> xs = [f x | f <- fs, x <- xs]
```

* Sources:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Applicative.html
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Control.Applicative.html
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Base.html#Applicative

## `Data.Semigroup`

Let's take a step away from boxed types and lifting function application
into the boxed type context. Let's just consider objects that have a
binary relation defined on them.

```haskell
-- From GHC.Base
class Semigroup a where
  -- Associative operation allowing two elements to be combined into
  -- one.
  (<>) :: a -> a -> a
```

As an example, in `Data.Semigroup`, they define a newtype `Min a`:

```haskell
-- newtype is a way of defining a synonymous class to `a` called
-- `Min a`. You produce one like `m = (5 :: Min Int)`. You can also
-- write `m = Min { getMin = 5 }`.
--
-- Then to get the minimum back out you write `x = getMin m`.
newtype Min a = Min { getMin :: a }

instance Ord a => Semigroup (Min a) where
  -- The function `min :: Ord a => a -> a -> a` is defined in
  -- `GHC.Classes` from `ghc-prim`. You use it like `min 3 4` to get
  -- `3`.
  --
  -- `coerce` is a function that allows you to safely convert between
  -- two types that have the same runtime representation.
  --
  -- This coerces `min` on `a` instances to the equivalent function on
  -- `Min a` instances.
  (<>) = coerce (min :: a -> a -> a)
```

This is allowing you to treat `a` as a semigroup with respect to the min
operation:

```haskell
import Data.Semigroup (Min(..))

mx = (3 :: Min Int)
my = (4 :: Min Int)
mz = mx <> my
z = getMin mz

main = putStrLn (show z)
```

There are many such helper newtypes defined in `Data.Semigroup`. Just a
note: it isn't always true that `x <> y == y <> x`. Even full groups
need not be commutative AKA Abelian.

Some other methods that are default defined in the `Semigroup` typeclass
for you: `sconcat`, which can be used to, e.g., find the min in a
`NonEmpty` list:

```haskell
-- Notice how this isn't a synonym for [a]. This is a different type.
-- Defined in GHC.Base, but not actually exported from Prelude.
-- It has a lot of functions defined in Data.List.NonEmpty but I'm not
-- interested right now.
data NonEmpty a = a :| [a]

-- Nothing special. By using NonEmpty ensures that sconcat cannot give
-- error. This is just a helpful method built on top of the Semigroup
-- `<>` function.
sconcat (a :| as) = go a as where
  go b (c:cs) = b <> go c cs
  go b []     = b
```

We're seeing a relationship between the concept of Semigroup and the
idea of reducing/multiplying/'concatenating' a series of values. We're
going to explore/generalize this concept when we talk about `Foldable`.

It also has an unusual method called `stimes :: Integral b => b -> a ->
a`. This 'repeats' the element many times. That is, if the element is x,
then `stimes 3 x` is `x \cdot x \cdot x`.

There are some possible implementations for the `stimes` class method in
`Data.Semigroup.Internal`:

```haskell
-- `x` if `n > 0`, else error
sTimesIdempotent n x

-- `mempty` if `n == 0`, else `x` if `n > 1`. Error if `n < 0`. Hint:
-- `mempty` is the identity value in a monoid.
sTimesIdempotentMonoid n x

-- Does a fancy thing where it knows that if `n` is even, it can halve
-- and say `y = sTimesMonoid (n `quot` 2) x`, and return `y \cdot y`.
-- Requires that `x` is a `Monoid`.
sTimesMonoid n x

-- Does the fancy thing, but errors if n == 0. Requires only that `x` is
-- a `Semigroup`.
sTimesDefault n x
```

* Sources:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Base.html#Semigroup
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.Semigroup.html
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.Semigroup.Internal.html
  * https://github.com/ghc/ghc/blob/master/libraries/ghc-prim/GHC/Classes.hs#L336

## `Data.Monoid`

`GHC.Base` defines a `Monoid`, which is basically just a `Semigroup`
with a neutral (identity) element:

```haskell
class Semigroup a => Monoid a where
  -- The neutral element like 0 or [] or "".
  mempty :: a

  -- The semigroup operation
  mappend = (<>)

  -- Extension of sconcat for any list
  mconcat :: [a] -> a
  mconcat = foldr mappend mempty
```

Examples:

```haskell
-- `[a]` is a semigroup under concatenation.
instance Semigroup [a] where
        (<>) = (++)
instance Monoid [a] where
        mempty  = []

-- Here's the Maybe monoid:
instance Semigroup a => Semigroup (Maybe a) where
    -- Just pass <> inside.
    Nothing <> b       = b
    a       <> Nothing = a
    Just a  <> Just b  = Just (a <> b)
-- But Nothing is the identity element then.
instance Semigroup a => Monoid (Maybe a) where
    mempty = Nothing

-- Ordering will be a semigroup in such a way that
instance Semigroup Ordering where
    LT <> _ = LT
    EQ <> y = y
    GT <> _ = GT

instance Monoid Ordering where
    mempty = EQ

-- Note that if you `mconcat` a list of Ordering elements, you'll
-- compute the lexicographic ordering value.
```

* Sources:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Base.html#Monoid
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.Monoid.html
    * Basically nothing interesting here.
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Monoid.html

## `Data.Foldable`

We've been talking `Monoid`s and `mconcat`, and applying it just to
`[a]`. But there is a wider world of objects that can be `mconcat`ed.
Thus we define the `Foldable` typeclass. Many of the methods are
generalizations of functions that apply to a list `[a]`.

```haskell
class Foldable t where
  -- Basically squashes a collection of Monoid objects.
  fold :: Monoid m => t m -> m
  fold = foldMap id

  -- Basically, you can fold even a container class of not a Monoid, so
  -- long as you first transform to a Monoid via the mapping `f`.
  --
  -- Monoid was needed first because the container might be empty.
  --
  -- Note that after seeing Functor and Applicative, this is our first
  -- way of getting something *out* of a container type.
  foldMap :: Monoid m => (a -> m) -> t a -> m
  -- Notice how `mappend . f` first transforms the element, then passes
  -- it as the first argument of the `mappend` function.
  foldMap = foldr (mappend . f) mempty

  -- `foldMap` generalizes `mconcat` beyond `[m]` to `t m`. But we can
  -- go further. Even for a simple `t a`, if you give your own operation
  -- `f :: a -> a -> a` (analogue of `mappend`) and a start value `x`
  -- (analogue of `mempty`), then `foldr` is your analogue of `foldMap`.
  --
  -- But that understates the case. The accumulator can be of type `b`.
  foldr :: (a -> b -> b) -> b -> t a -> b
  -- For your own container classes, will need to define `foldr`
  -- yourself. This is specific to your class. Note that `foldr` does
  -- not merely squash, so you don't need either `a` nor `b` to be a
  -- Monoid.

  -- TODO: Review various other versions of `foldl`, `foldr'`?
```

There are a number of other derived functions:

```haskell
class Foldable t where
  -- Specialization of fold
  toList :: t a -> [a]
  toList = foldr : []

  -- Likely you'll choose to optimize this?
  null :: t a -> Bool
  null = foldr (\_ _ -> False) True

  length :: t a -> Int
  length = foldr (\_ c -> c+1) 0

  elem :: Eq a => a -> t a -> Bool
  elem x xs = any (== x)

  -- min and max are defined using the Semigroups we saw before.
  -- If `a` is `Ord`, then `Maybe (Ord a)` is a monoid. So we `foldMap`
  -- with a transform of `a` into `Just (Min a)`.

  -- Similarly we can define `sum` and `product` since if `a` is `Num`,
  -- then `Sum a` is a Monoid. We can `foldMap` with a transform of
  -- `a` to `Sum a` or `Product a`.
```

Let's see some instances/examples:

```haskell
instance Foldable Maybe where
    -- maybe defined in Data.Maybe. `maybe :: b -> (a -> b) -> Maybe a -> b`.
    -- In other words: `foldMap` will apply the function to transform
    -- `a` into the Monoid `b`, but will return `mempty :: b` if there
    -- is the `Maybe a` is in fact `Nothing`.
    --
    -- This is for efficiency because we're about to define `foldr`.
    foldMap = maybe mempty

    -- Does exactly what you expect.
    foldr _ z Nothing = z
    foldr f z (Just x) = f x z

instance Foldable [] where
    foldr   = List.foldr

and :: Foldable t => t Bool -> Bool
-- They write a version with `#.` and I don't know what that means...
and bools = getAll (foldMap All bools)

all :: Foldable t => (a -> Bool) -> t a -> Bool
-- Again, my rewrite eliminates `#.`.
all f xs = getAll (foldMap (All . f) xs)

-- There are further functions for or/any.

-- Note: there is no explicit shortcut to terminate early for
-- `and`/`all`. Does that mean the entire list is iterated? Maybe not?
-- Because maybe lazy evaluation, and as soon as we hit a `False` value,
-- the other half of the `mappend` action (which is `&&`) is known as
-- not needed and is thus never continued? In fact, I believe this is
-- *exactly* what happens. Wow.

find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find p xs = getFirst (foldMap f xs)
  where
    f x = First (if p x then Just x else Nothing)
```

Let's look at one last thing: the definition for a tree:

```haskell
-- Definition of my tree type
data Tree a = Empty | Node (Tree a) a (Tree a)
  deriving Show

-- Interesting. It wants `Tree` rather than `Tree a`. It wants to work
-- on a kind `* -> *`, AKA a container of any kind of thing. It doesn't
-- want to apply to just a specific kind of container.
instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node leftSubtree x rightSubtree) =
    leftFoldValue
    `mappend` mValue
    `mappend` rightFoldValue
    where
      leftFoldValue = foldMap f leftSubtree
      mValue = f x
      rightFoldValue = foldMap f rightSubtree
```

* Sources:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.Foldable.html

## `Control.Monad`

We've previously generalized `Functor` to `Applicative`. But we could
have generalized in another direction. We could have asked: if a
function `f :: a -> m b` takes in an `a` and produces a monad wrapped
value of type `m b`, then how do we apply such a function to an already
wrapped value of type `m a`?

That is: `f` produces a boxed value. It's no good to try `f <$> boxedA`,
since then this would just produce a value of type `m (m b)`. Check it
out:

```haskell
Prelude> f :: Int -> IO Int; f x = pure (x + 1)
Prelude> ioX :: IO Int; ioX = pure 3
Prelude> :t (f <$> ioX)
(f <$> ioX) :: IO (IO Int)
-- Note: ghci will perform IO once for you, but not recursively.
Prelude> import System.IO.Unsafe (unsafePerformIO)
Prelude System.IO.Unsafe> unsafePerformIO (f <$> ioX)
4
```

Let's consider our standard examples of `[]`. How can we apply `f :: a
-> [b]` to a `[a]`? We could `fmap f listOfAs` to produce
`listOfListOfBs :: [[b]]`. Since `[]` is a `Monoid`, we can then call
`mconcat listOfListOfBs` to collapse to `listOfBs :: [b]`. This approach
should be appropriate for anything which is:

1. A `Functor` (so we can call `fmap`),
2. A `Monoid` (so that we can call `mappend` on a list of them to collapse),
3. A `Foldable` (so that we can call `fold`, which will call `mappend`;
   basically, we need to be able to treat the object like a list of
   things).

TODO: But I don't actually see an implementation for everything with
those type constraints? Why?

Anyway, let's look at the `Monad` typeclass:

```haskell
-- From GHC.Base
class Applicative m => Monad m where
  -- A way to lift a function into the monad, but not 'double-box' the
  -- result.
  (>>=) :: m a -> (a -> m b) -> m b

  -- A convenience built on >>= but ignores the previous value.
  (>>) :: m a -> m b -> m b
  ma >> mb = ma >>= \_ -> mb

  -- return is just a synonym for pure
  return = pure
```

And let's see some implementations:

```haskell
-- Unbox and apply. Like `fmap`, but, as ever, avoiding the double
-- boxing.
instance Monad Maybe where
  (Just x) >>= f = f x
  Nothing >>= _ = Nothing

instance Monad [] where
  -- Use a list comprehension to produce a flattened list.
  xs >>= f = [y | x <- xs, y <- f x]
```

You can use `do` notation in place of `return` and `>>=`. Consider this
scenario:

```haskell
-- Squares a number, but doesn't let it get too big.
squareIfNotTooBig :: Int -> Maybe Int
squareIfNotTooBig n = if squareN < 40 then Just squareN else Nothing
  where
    squareN = n * n

-- Quintuples a number, but doesn't let it get too big.
quintupleIfNotTooBig :: Int -> Maybe Int
quintupleIfNotTooBig n = if fiveN < 40 then Just fiveN else Nothing
  where
    fiveN = 5 * n

-- Tries to do both, but returns Nothing if either calculation failed.
doBoth :: Int -> Maybe (Int, Int)
doBoth n =
  (squareIfNotTooBig n) >>=
  \squareN -> (quintupleIfNotTooBig n) >>=
  \fiveN -> return (squareN, fiveN)

-- Same thing, written more simply
doBoth n = do
  squareN <- squareIfNotTooBig n
  fiveN <- quintupleIfNotTooBig n
  -- return boxes it in a Just for us.
  return (squareN, fiveN)
```

It's a little weird, but note that this would work:

```haskell
import Data.Function ((&))

xs = [10, 20, 30]

f x = [x, 2*x, 3*x]

ys = do
  -- Note that `xs` contains *many* `x` values, not just one.
  x <- xs
  -- The list will be reduced
  f x

-- ys == [10, 20, 30, 20, 40, 60, 30, 60, 90]
```

But then you also have some weirdness:

```haskell
-- Fine, works normally and just ignores the single `y` value.
ys = do
  x <- xs
  -- Could also simply write [1]. This is the same, but doesn't make the
  -- monad value available to succeeding computations.
  y <- [1]
  f x

-- Equivalent to:
ys = xs >>= (\x -> [1] >>= (\_ -> f x))
-- Or also equivalent, since we don't use the `y` value
ys = xs >>= (\x -> [1] >> f x)

-- This seeming irrelevancy forces ys == []
ys = do
  x <- xs
  y <- []
  f x

-- Equivalent to:
ys = xs >>= (\x -> [] >>= (\_ -> f x))
ys = xs >>= (\x -> [] >> f x)

-- Will cause each f x to be repeated twice.
ys = do
  x <- xs
  y <- [1, 2]
  f x
```

The `[]` monad is odd because the same function might be called multiple
times with different input values. Which is quite unlike `IO` monad,
where the continuation function is called at most once (exactly once if
no preceding step failed).

The Learn You A Haskell people talk about how list comprehensions are
syntactic sugar for `do` syntax. I've shown this. But there can also be
*guards* in list comprehension:

```haskell
-- Only return those numbers with a 7 in the base 10 representation.
[ x | x <- [1..50], '7' `elem` show x ]
```

How is this accomplished? The list is actually an instance of
`MonadPlus`. `MonadPlus` means a `Monad` which is also a `Monoid`. In
particular, there's an `mzero` function in the typeclass for
`MonadPlus`: it's the equivalent of the `Monoid` `mempty` function. In
the case of `[]`, this is the empty list. You can see the definition for
`guard` in the `Control.Monad` source file.

Here's my implementation of Knight's Travails.

```haskell
import Data.Function ((&))
import Control.Monad (guard)

type Position = (Int, Int)

isValidPosition :: Position -> Bool
isValidPosition (x, y) = 1 <= x && x <= 8 && 1 <= y && y <= 8

nextPositions :: [Position] -> [Position]
nextPositions currentPositions = do
  (x, y) <- currentPositions
  nextPosition <- [(x - 2, y - 1), (x - 2, y + 1),
                   (x - 1, y - 2), (x - 1, y + 2),
                   (x + 1, y - 2), (x + 1, y + 2),
                   (x + 2, y - 1), (x + 2, y +1)]
  -- Notice the use of guard to fail any position that is not valid.
  guard (isValidPosition nextPosition)
  return nextPosition

reachablePositions :: [Position] -> Int -> [Position]
reachablePositions currentPositions 0 = currentPositions
reachablePositions currentPositions numSteps = do
  reachablePositions (nextPositions currentPositions) (numSteps - 1)

isReachable :: Position -> Position -> Int -> Bool
isReachable startPosition endPosition maxNumSteps =
  endPosition `elem` (reachablePositions [startPosition] maxNumSteps)

main = do
  putStrLn ((isReachable (1, 1) (8, 8) 7) & show)
```

We've seen that `monadicValue >>= f` feeds the value into the function
`f :: a -> b`. And of course we can write: `monadicValue >>= f >>= g`.
But is there a way to "compose" `f, g` easily, like we did with `f . g`
for normal functions?

Answer: the analogue is `f >=> g`. We can write: `monadicValue >>= (f >=> g)`.

* Source:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Base.html#Monad
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Control.Monad.html

## Writer Monad

The `Writer` monad is defined in the `transformers` package. It's
written in a way that is inscrutable to me presently.

The idea of the `Writer` monad is that it works on a `Monoid`
'accumulator', and the `>>=` operation works by fusing the two
accumulators through `mappend`.

```haskell
-- Allow re-declaration of instance signatures to help me debug.
{-# Language InstanceSigs #-}

import Data.Function ((&))

-- Great naming. `w` means the accumulator value. `a` means the kind
-- of value wrapped by the monad.
newtype Writer w a = Writer { runWriter :: (a, w) }

-- This seems pretty clear. If you want to transform the wrapped
-- value without writing into the accumulator, go for it. Note: we write
-- just `Writer w` without any kind of wrapped value type `a` because
-- Functor has kind * -> *. That makes sense, because the whole point is
-- that the wrapped value type will change after function application.
instance Functor (Writer w) where
  fmap :: (a -> b) -> (Writer w a) -> (Writer w b)
  f `fmap` wrappedX = Writer (y, ctx)
    where
      (x, ctx) = runWriter wrappedX
      y = f x

instance Monoid w => Applicative (Writer w) where
  -- It's easy to lift a value into the Writer (wrap it with the
  -- writer), so long as there is a `mempty` value that represents a
  -- natural start state for the accumulator.
  pure :: a -> (Writer w a)
  pure x = Writer (x, mempty)

  -- TODO: This seems very weird. Not sure I understand what this should
  -- be doing....
  (<*>) :: (Writer w (a -> b)) -> (Writer w a) -> (Writer w b)
  wrappedF <*> wrappedX = error "What the fuck?"

-- Same note about use of `Writer w`: `>>=` will transform the wrapped
-- type.
instance Monoid w => Monad (Writer w) where
  (>>=) :: (Writer w a) -> (a -> Writer w b) -> (Writer w b)
  wrappedX >>= f = Writer (y, oldCtx `mappend` newCtx)
    where
      (x, oldCtx) = runWriter wrappedX
      (y, newCtx) = runWriter (f x)

-- Just a convenient synonym.
type Logged x = Writer [String] x

addOne :: Int -> Logged Int
addOne x = Writer (x + 1, ["Added one"])

double :: Int -> Logged Int
double x = Writer (x + x, ["Doubled"])

-- Do a series of computations, collecting up the list of logs.
doSomeMath :: Int -> Logged Int
doSomeMath x = do
  x1 <- addOne x
  x2 <- double x1
  x3 <- addOne x2
  return x3

main = do
  putStrLn ((doSomeMath 10) & runWriter & show)
```

* Sources:
  * The `Writer` monad is written in terms of monad transformers. It is
    defined in the `transformers` package, which is a dependency of the
    `mtl` package. I believe the `mtl` package extends `trasformers`.
  * Here is the source code for `Writer`'s monad implementation:
  * https://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.Writer.Lazy.html#line-197
  * It's hard to read the `Writer` definition because it is written in
    terms of `WriterT`, which is a *monad transformer*. I don't know
    what a monad transformer is, though.
  * TODO: learn about monad transformers!

## State Monad

The `Writer` monad is a little simplistic. The context for a value is
updated simply through concatenation. We can produce new context values,
which are reduced into the current accumulation through `mappend`.

We don't have the ability to ever look at the context that has been
collected along the way. We must wait until someone calls `runWriter` to
'cash out' the state at the end.

Thus, it is entirely appropriate that we call `Writer` 'writer.' It lets
us *write* into a log, which we cannot read at any intermediate step. In
fact, it's really just an 'append only' writing.

Thus, we invent the `State` monad!

```haskell
-- Allow re-declaration of instance signatures to help me debug.
{-# Language InstanceSigs #-}

import Data.Function ((&))

-- Basically, the `State` is 'unmaterialized' in the sense that only
-- when you feed in the initial state can you then run things forward
-- to get the current value `a` and also the current underlying
-- context `s`.
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> (State s a) -> (State s b)
  f `fmap` currentState = State $ \initialStateVal -> (
    let (currentVal, currentStateVal) = runState currentState initialStateVal
        nextVal = f currentVal
    in
      (nextVal, currentStateVal)
    )

instance Applicative (State s) where
  pure :: a -> (State s a)
  pure x = State $ \s -> (x, s)

  -- TODO: I'm too dumb to figure out what <*> means in this case?
  --
  -- It looks like Haskell lets you off with a warning for unimplemented
  -- methods...

instance Monad (State s) where
  return :: a -> (State s a)
  return = pure

  (>>=) :: (State s a) -> (a -> State s b) -> State s b
  currentState >>= f = State $ \initialStateVal -> (
    let (currentVal, currentStateVal) = runState currentState initialStateVal
        (nextVal, nextStateVal) = runState (f currentVal) currentStateVal
    in
      (nextVal, nextStateVal)
    )

type Stack = [Int]

pop :: State Stack Int
pop = State $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push x = State $ \xs -> ((), x:xs)

manipulateStack :: State Stack ()
manipulateStack = do
  x <- pop
  push (x * 2)
  push (x * 3)
  push (x * 4)

main = do
  putStrLn (stack & show)
  where
    (_, stack) = runState manipulateStack [10]
```

There are two helpers defined in `Control.Monad.State`:

```haskell
get :: State stateType stateType
get = \stateVal -> (stateVal, stateVal)

put :: newStateType -> State () oldStateType
put newStateVal = \oldStateVal -> ((), newStateVal)
```

The name `State` is a bit of a misnomer. The value `manipulateStack` is
a good example. It looks more like a value rather than a function. It is
sort-of hiding that the `State` returned by `manipulateStack` actually
needs to have a `stateValue` injected into it in order to be run. Only
then can a resulting value and state value be returned.

Perhaps a better name would have been something like
`StatefulInteraction`. A `State` is describing *what to do*, not a
specific state at any moment in time.

It is also interesting to reflect that inside the `do` syntax we do have
something akin to a side-effect. This is because the the monad's context
vlaue, which is evolving through the calls to `push`/`pop`, is hidden
from us.

```haskell
-- You may have to install random package. `stack install random`
import qualified System.Random as R
import Control.Monad.State (runState, State, state)

-- This is the signature of the random function.
--
-- random :: (RandomGen g, Random a) => g -> (a, g)
--
-- Basically, it takes a `RandomGen`, gets the next random bytes,
-- deserializes a new instance of `a`, and then returns the value with
-- the new `RandomGen` state.

-- Remember that `getNextRandom`, despite being a `State g a`, means
-- that it is a description of how to take the old state, and produce
-- a new state along with the value of type `a`. So `getNextRandom` is
-- kind of a function even though it looks like a constant.
getNextRandom :: (R.RandomGen g, R.Random a) => State g a
-- Note that I use the `state` function to create a `State`
-- value. This has been the way to produce a `State` ever since monad
-- transformers were introduced. LYAH is out-of-date in this respect.
--
-- Note that `state` is, of course, passed the function `R.random` that
-- takes in a state value and produces both a value and a state value.
-- That's exactly what a `State` is: a kind of function.
getNextRandom = state R.random

-- Again, note that this is a `State` value. Again, it is a
-- *description* of how to produce the new state and three bools.
getThreeRandomBools :: State R.StdGen (Bool, Bool, Bool)
getThreeRandomBools = do
  bool1 <- getNextRandom
  bool2 <- getNextRandom
  bool3 <- getNextRandom
  return (bool1, bool2, bool3)

main = do
  putStrLn (show boolsRoundOne)
  putStrLn (show boolsRoundTwo)
  where
    initialStdGen = (R.mkStdGen 0)
    ((boolsRoundOne, boolsRoundTwo), _) = runState (
      do
        boolsRoundOne <- getThreeRandomBools
        boolsRoundTwo <- getThreeRandomBools
        return (boolsRoundOne, boolsRoundTwo)
      ) initialStdGen
```

* Source
  * The `State` monad is defined here:
  * https://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.State.Lazy.html#line-221

## Summary of Types Encountered

* Reducable/combineable things:
  * `Semigroup`: has an associative binary operation.
  * `Monoid`: a semigroup with an identity element. Examples are `Sum`,
    `Product`, `[]`, `Maybe`.
* Things that can interact with functions:
  * `Functor`: anything that is a box of a value where a function can be
    lifted to the box-level and applied.
  * `Applicative`: where a box of functions can be applied to an instance of
    the box.
      * TODO: not really sure of any great examples of something that
        feels very applicative to me?
      * `[]` is a natural example of `Applicative`.
  * `Foldable`: anything that can iterate its items. Anything with a
    `toList` method. Anything with a `foldr` method.
      * `fold` and `foldMap` are simple methods that can work when the
        `Foldable` contains `Monoid` values, or are mapped to some
        `Monoid`.
* Monads
  * `Maybe`: context is simply 'have I failed?' `>>=` threads the
    current value to the next function, but will ignore failed values.
    The function can succeed or fail.
    * In `do` notation, as soon as `Nothing` appears, then there is no
      recovery. `Nothing` must be returned. You could call this a
      'side-effect,' in that the use of the `Nothing` monad changes the
      context.
  * `[]` monad?
  * `Writer`: keeps track of a `Monoid` state. `>>=` threads the
    current value to the next function. The function returns both the
    next value and anything to `mappend` into the monad context.
      * There is no way for the monad to read the accumulated context.
      * `Int`, `""`, and `[]` are all good choices for a context.
      * In `do` notation, you might encounter a `Writer` value that
        simply writes a log-line. It might *look* like nothing, but it a
        monadic action in the sequence described by `do`.
  * `State`: broadens the allowed interaction with the state. `>>=`
    threads both the current value *and* the current state.
      * In this way, it is strictly more general than `Writer`.

## TODO

* As we approach the thrilling conclusion, there are a few LYAH pages:
  * http://learnyouahaskell.com/for-a-few-monads-more
    * Read up to error error.
  * http://learnyouahaskell.com/zippers
* After reading these pages, I should probably do a thorough
  investigation into `GHC.Base` and `Control.Monad` to review the
  various functions that are defined therein.
* Years ago, I claimed that I saw (and wrote) the `State` monad. If
  that's true, could I explain it here today?

Haskell books dump:

I already (re)read Learn You a Haskell recently...

* http://www.apress.com/9781430262503
* http://www.cambridge.org/us/academic/subjects/computer-science/programming-languages-and-applied-logic/thinking-functionally-haskell
* http://chimera.labs.oreilly.com/books/1230000000929
* http://www.cs.nott.ac.uk/~gmh/book.html
* http://www.haskellcraft.com/craft3e/Home.html
* http://www.yesodweb.com/book
* https://www.haskell.org/tutorial/
* https://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Preamble
* https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
* https://wiki.haskell.org/Haskell
* http://dev.stephendiehl.com/hask
* http://www.vex.net/~trebla/haskell/index.xhtml
* https://wiki.haskell.org/Learning_Haskell
* https://wiki.haskell.org/How_to_write_a_Haskell_program
* https://wiki.haskell.org/Typeclassopedi
* https://news.ycombinator.com/item?id=10008169

* Monad transformers??
