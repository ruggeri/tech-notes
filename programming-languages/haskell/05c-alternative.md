## `Alternative`, `guard`, `MonadPlus`

First, we need to generalize the concept of boolean value. This is the
purpose for `Alternative`.

```haskell
-- Defined in GHC.Base
class Applicative f => Alternative f where
  -- `empty` corresponds to `False`.
  empty :: f a

  -- `<|>` corresponds to `||`
  (<|>) :: f a -> f a -> f a
```

An example implementation:

```haskell
instance Alternative Maybe where
  -- Nothing is falsey
  empty = Nothing

  -- Else just take the first non falsey value.
  Nothing <|> r = r
  l <|> _ = l

-- Same concept for []
instance Alternative [] where
  -- Empty list is falsey
  empty = []

  [] <|> r = r
  l <|> _ = l
```

Isn't anything that is an `Alternative` thus also a `Monoid`? I believe
the answer is yes. However, if `a` is a `Semigroup`, then we want to be
able to treat `Maybe a` as a monoid, and have:

```haskell
instance Semigroup a => Monoid (Maybe a) where
  mempty = Nothing

  Nothing `mappend` r = r
  (Just x) `mappend` (Just y) = Just (x `mappend` y)
```

My point is: yes `Alternative` is defining a monoid structure, but it's
very basic. The monoid operation is simply to return the first non-null
value. Whereas we frequently want to also consider a richer monoid where
non-null operations have a semigroup operation performed between them.

## `guard`, `when`, `unless`

```haskell
-- Defined in Control.Monad. Simply turns a Bool into a truth value.
guard :: Alternative f => Bool -> f
guard True = pure ()
guard False = empty

-- Generalizes guard to Applicative. Whereas `guard` is intended to kill
-- the computation so that no further action occurs, this simply will
-- give a no-op value if p is false. Else, it will return an action.
when :: Applicative f => Bool -> f () -> f ()
when p f = if p then f else pure ()
```

## `MonadPlus`

In our Knight's Travails solution, we used `guard` in a context where
the result in a `Monad`. We have a special name for when something is
both an `Alternative` and a `Monad`:

```haskell
-- Defined in GHC.Base
class (Alternative m, Monad m) => MonadPlus m where
  -- The rule is that `mzero >>= f` should equal `mzero`. In effect,
  -- `mzero` is an end state of the monad.
  mzero :: m a
  mzero = empty

  -- mplus is merely a synonym for the <|> method that generalizes ||.
  mplus :: m a -> m a -> m a
  mplus = (<|>)
```

* Sources:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Base.html#Alternative
