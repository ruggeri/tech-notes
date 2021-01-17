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
