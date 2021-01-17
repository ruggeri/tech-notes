# `Data.Monoid`

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
-- [a]
instance Semigroup [a] where
  -- `[a]` is a semigroup under concatenation.
  (<>) = (++)
instance Monoid [a] where
  -- And the empty list is `mempty`.
  mempty  = []

-- Semigroup a => Maybe a
instance Semigroup a => Semigroup (Maybe a) where
  -- Just pass <> inside. This is why `a` must be `Semigroup`
  Nothing <> b       = b
  a       <> Nothing = a
  Just a  <> Just b  = Just (a <> b)
instance Semigroup a => Monoid (Maybe a) where
  -- Nothing is `mempty`.
  mempty = Nothing

-- Ordering will be a semigroup in such a way that, if you `mconcat` a
-- list of Ordering values, you'll compute the lexicographic ordering
-- value.
instance Semigroup Ordering where
    LT <> _ = LT
    EQ <> y = y
    GT <> _ = GT

instance Monoid Ordering where
    mempty = EQ
```

* Sources:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Base.html#Monoid
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.Monoid.html
    * Basically nothing interesting here.
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Monoid.html
