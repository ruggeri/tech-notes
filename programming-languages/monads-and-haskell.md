
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

  foldr :: (a -> b -> b) -> b -> t a -> b
  -- You will need to define `foldr` yourself. This is specific to your
  -- class. Note that `foldr` does not merely squash, so you don't need
  -- `b` to be a Monoid.

  -- TODO: Review various other versions of `foldl`, `foldr'`
```

There are a number of other derived functions:

```haskell
class Foldable t where
  -- Specialization of fold
  toList :: t a -> [a]
  toList = foldr : []

  null = foldr (\_ _ -> False) True

  length = foldr (\_ c -> c+1) 0

  elem :: x ->
  elem x xs = any (== x)
```

* Sources:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.Foldable.html

**TODO**

* Foldable
    * Implements things you can call `foldr` on.
    * Writes in terms of a `foldMap :: (Foldable f, Monoid m) => (a ->
      m) -> f a -> m`.
    * For a tree this would look like:

```
foldMap f Empty = mempty
foldMap f (Tree x l r) = foldMap f l `mappend`
                         f x `mappend`
                         foldMap f r
```

    * Note that you could easily use this to build an array
      representation of the tree. But it would also be easy to write:

```
getAny $ foldMap (\num Any (odd num)) myBigNumbersTree
foldMap (\num [num]) myBigNumbersTree
```

* Monads
    * `>>= :: (Monad m) => m a -> (a -> m b) -> m b`
    * This is exactly what is needed for IO, of course.
    * Also `>> :: (Monad m) => m a -> m b -> m b`.
    * Basically, sequences, but ignores previous.
    * `return :: (Monad m) => a -> m a`; this boxes a value.
    * We can think of this as an append log of commands.
    * Another possibility with lists; we can explore more and more
      possibilities, as in the knight's travails.
    * In lists, `>>= :: [a] -> a -> [b] -> [b]` and this is done by
      `>>= xs f = concat (map f xs)`.
* Writer
    * Equivalent to `newtype Writer m a = (a, m)`, where `m` is a
      monoid. It allows you to do computation while building up a log.

```
(Monoid m) => Monad (Writer m a) where
    (Writer (x, v)) >>= f = let (Writer y, v') = f x in (Writer y (v `mappend` v'))
```

* Also I saw (and wrote!) the State monad.

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
