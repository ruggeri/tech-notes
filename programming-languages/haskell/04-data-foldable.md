# `Data.Foldable`

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
  -- `m` must be a `Monoid` because the container might be empty.
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
  toList = foldr (:) []

  -- Returns whether `t` is empty. Likely you'll choose to optimize
  -- this?
  null :: t a -> Bool
  null = foldr (\_ _ -> False) True

  length :: t a -> Int
  length = foldr (\_ c -> c+1) 0

  -- This uses a language extension to say: only provide maximum if `a`
  -- is `Ord. Note that this appears in the typeclass to allow
  -- specialization.
  maximum :: forall a. Ord a => t a -> a
  maximum xs =
    fromMaybe
      -- Default value is actually going to raise an error!
      (errorWWithoutStackTrace "maximum: empty structure")
      -- They use a Max from Data.Functor.Utils. Here Max is a newtype
      -- for Maybe a, where a is `Ord`. `Max a` is a `Monoid`, whereas
      -- the version from `Data.Semigroup` is merely a `Semigroup`.
      --
      -- Thus, they map each x to `Max (Just x)`, fold the monoid values
      -- and finally extract.
      getMax . foldMap (Max . (Just :: a -> Maybe a)) xs

  -- It is easier to define `sum` and `product`, since in Data.Semigroup
  -- they define `mempty` to be `Sum 0`/`Product 1`. These are thus
  -- Monoid and immediately foldable.

  -- `any` will be defined below
  elem :: Eq a => a -> t a -> Bool
  elem x xs = any (== x) xs

-- Any and All are defined in Data.Semigroup. They are newtypes for
-- Bool. They `<>` in the obvious way, with an appropriate `mempty`
-- value.
--
-- Note that because `&&` and `||` won't evaluate the second argument if
-- not-needed, `any` (and `all`) will stop execution as soon as they hit
-- a True (or False) value.
any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = getAny . foldMap (Any . p)
all :: Foldable t => (a -> Bool) -> t a -> Bool
all f xs = getAll (foldMap (All . f) xs)

-- `and`/`or` are defined even more simply.
and bools = getAll (foldMap All bools)
or bools = getAll (foldMap Any bools)

-- Uses the First monoid.
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find p xs = getFirst (foldMap f xs)
  where
    f x = First (if p x then Just x else Nothing)
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
