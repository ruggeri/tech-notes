(To run these programs, you can `stack runghc script.hs`).

Haskell uses monads for IO. Basically, it builds up an object tha
represents the transactions that should take place throughout the
program.

Do notation is just a shorthand to make this simpler.

I see this as kind of like CPS, actually.

## Common Haskell Classes

**`Data.Functor`**

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
xs = (show :: Int -> String) <$> [1, 2, 3]

main = do
  -- Prints ["1", "2", "3"]
  putStrLn (show xs)
```

Another example is `IO`:

```haskell
-- I'll cover, do and <- and return properly later.
myFmap :: (a -> b) -> IO a -> IO b
myFmap f action = do
  x <- action
  return (f x)

readStringAndAppendMsg = fmap (++ " is the input string") getLine

main = do
  putStrLn "Please type input string"
  s <- readStringAndAppendMsg
  putStrLn s
```

* Sources:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Functor.html
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Base.html#Functor
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.Functor.html

**`Control.Applicative` Functor**

We say 'Applicative Functor,' but that class is named `Applicative`. An
`Applicative` extends the concept of a `Functor` by allowing that the
`Functor` might wrap a function, which in turn can be applied.

```haskell
class Functor f => Applicative f where
    -- Lifts a value into the Applicative.
    pure :: a -> f a
    -- Takes a function wrapped in an Applicative and applies it to
    -- another applicative.
    (<*>) :: f (a -> b) -> f a -> f b
```

Here's an example with `IO`:

```haskell
import Data.Functor

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
-- thus a lifted version of &.
main :: IO ()
main = (doubleOrTripleThree <&> show) >>= putStrLn
```

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

There is a synonym of `<*>`, `liftA2`:

```haskell
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

**Semigroup**

```haskell
-- From GHC.Base
class Semigroup a where
  -- Associative operation allowing two elements to be combined into
  -- one.
  (<>) :: a -> a -> a
```

In `Data.Semigroup`, they define a newtype `Min a`:

```haskell
-- newtype is a way of defining a synonymous class to `a` called
-- `Min a`. `getMin` is an accessor function. But `Min a` is the way to
-- convert back to the underlying type `a`.
newtype Min a = Min { getMin :: a }

instance Ord a => Semigroup (Min a) where
  -- so `min :: Ord a => a -> a -> a`. So `min 3 4` works from
  -- `GHC.Classes` in `ghc-prim`.
  --
  -- `coerce` is a function that allows you to safely convert between
  -- two types that have the same runtime representation.
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
note: it isn't always true that `x <> y == y <> x`.

Some other methods that are default defined in the `Semigroup` class for
you: `sconcat`, which finds the min in a `NonEmpty`:

```haskell
-- Notice how this isn't a synonym for [a]. This is a different type.
-- Defined in GHC.Base, but not actually exported from Prelude.
data NonEmpty a = a :| [a]

-- Nothing special. By using NonEmpty ensures that sconcat cannot give
-- error.
sconcat (a :| as) = go a as where
  go b (c:cs) = b <> go c cs
  go b []     = b
```

It also has an unusual method called `stimes :: Integral b => b -> a ->
a`. This 'repeats' the element many times. That is, if the element is x,
then `stimes 3 x` is `x \cdot x \cdot x`.

There are some possible implementations for the `stimes` class method in
`Data.Semigroup.Internal`:

```haskell
-- `x` if `n > 0`, else error
sTimesIdempotent n x

-- `mempty` if `n == 0`, else `x` if `n > 1`. Error if `n < 0`
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

**Monoids**

`GHC.Base` defines a `Monoid`, which is basically just a `Semigroup`
with a neutral element:

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

Another example is `Ordering`. `Ordering` can be extended to do
lexicographic comparison of lists of `Order` if we treat it as a
semigroup. And it can even compare empty lists of things if we treat it
as a `Monoid` where `mempty = EQ`.

* Sources:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Base.html#Monoid
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.Monoid.html
    * Basically nothing interesting here.
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Monoid.html

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
