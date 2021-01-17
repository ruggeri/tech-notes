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
-- write `m = Min { getMin = 5 }` or `m = Min 5`.
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

-- Has a constraint of Monoid. `mempty` if `n == 0`, else `x` if `n > 1`.
-- Error if `n < 0`.
sTimesIdempotentMonoid n x

-- Does a fancy thing. If `n` is even, calculates
-- `sTimesDefault (n `div` 2) x` and then squares that. Error if
-- `n <= 0`.
sTimesDefault n x

-- Has a constraint of Monoid. Will allow `n = 0` in which case it uses
-- `mempty`.
sTimesMonoid n x
```

* Sources:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Base.html#Semigroup
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.Semigroup.html
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.Semigroup.Internal.html
  * https://github.com/ghc/ghc/blob/master/libraries/ghc-prim/GHC/Classes.hs#L336
