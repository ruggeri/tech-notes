# Folding

## List `foldr`, `foldl`, `foldl'`

**`foldr`**

```haskell
-- `foldr` for `[a]` is defined in GHC.Base.
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Base.html#foldr
--
-- My version is only ever-so-slightly different than the real GHC.Base
-- code.
--
-- `foldr` 'associates to the right.' That is, it computes:
--
--   x1 `f` (x2 `f` (... (xn `f` y)...))
--
-- If `f` can ever ignore its second argument, then `foldr` can
-- converge even on an infinite list.
--
-- On the other hand, `foldr` is not tail-recursive. Thus, when
-- evaluating `foldr`, it will evaluate a tower of `n` thunks.
--
-- Random thought: if f is building a list by concatenation, and the
-- result is consumed bit-by-bit, you might be able to slowly pull apart
-- the tower of thunk, evaluating just a bit at a time as needed.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f y [] = y
foldr f y (x:xs) = f x (foldr f y xs)
```

**`foldl`**

```haskell
-- In Data.List, they define `foldl` for a list.
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.List.html#foldl
--
-- `foldl` is going to associate *left*. Which means
--
--   ((...((z0 `f` x1) `f` x2) `f` ...) `f` xn)
--
-- Here is my version first. It could take O(1) memory because it is
-- implemented tail recursively. However, you should note that even if
-- we ever accumulate a value that is a fixed point, we'll still keep
-- folding until the end.
--
-- This version actually still takes O(n) space because, despite the
-- tail recursion, the accumulator value is lazily evaluated. Thus a
-- thunk is passed into the recursive call. If you iterate `n` elements,
-- you create a nesting of `n` thunks.
--
-- We can fix it by forcing strict evaluation.
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f y [] = y
foldl f y (x:xs) = foldl f (f y x) xs

-- Here is the `foldl` version from GHC.List. Wow, unusually helpful
-- annotations on this function!
--
-- So, the function is written in terms of `foldr`. It will fold to
-- create a function of type `z -> z`. It will, finally, apply this to
-- the initial accumulator value, `z0`.
--
-- I'm not sure if there's a reason to write `foldl` like this rather
-- than my way. Perhaps simply to re-use `foldr`?
foldl :: forall a b. (b -> a -> b) -> b -> [a] -> b
foldl k z0 xs =
  foldr
    -- `fn` is a hole that will contain the recursive foldr result.
    -- It is not immediately needed. `k z v` can be calculated first.
    --
    -- For this reason, foldl *could* take O(1) space. Except that
    -- `k z v` is lazily evaluated. So the recursive `foldr` call is
    -- passed a thunk! And, thus our nested thunks build up.
    --
    -- Note that `fn` will always be, eventually, evaluated. This just
    -- proves that `foldl` will diverge for any infinite list.
    (\(v::a) (fn::b->b) -> \(z::b) -> fn (k z v))
    (id :: b -> b)
    xs
    z0
```

**`foldl'`**

```haskell
-- From Data.list:
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.List.html#foldl%27
--
-- This version works similarly to the prior version of `foldl`. There
-- is an essential difference, however. It is this:
--
--   z `seq` fn (k z v)
--
-- The `seq` function forces evaluation of `z` before invoking `fn`.
-- This prevents the accumulator value from becoming a tower of thunks.
--
-- In this way, we finally get our desired O(1) space complexity!
foldl'           :: forall a b . (b -> a -> b) -> b -> [a] -> b
foldl' k z0 xs =
  foldr
    (\(v::a) (fn::b->b) -> \(z::b) -> z `seq` fn (k z v))
    (id :: b -> b)
    xs
    z0
```

## Folding Monadic Values

**`foldM`**

```haskell
-- From GHC.Base:
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Control.Monad.html#foldM
--
-- foldM is an alias for foldlM
foldM = foldlM
```

**`foldlM`**

```haskell
-- From Data.Foldable:
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.Foldable.html#foldlM
--
-- This is written using `foldr`, as `foldl` was, too. It uses the
-- common trick of using `foldr` to build a function, which is then
-- applied to the initial accumulator `z0`.
foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM f z0 xs = foldr c return xs z0
  -- Similar to the List definition of foldl. `k` represents the fold
  -- for everything to the right of the present value (how to finish the
  -- fold). `x` represents the current value. And `z` represents the
  -- result of folding elements to the left. `z` is only provided at the
  -- very end, when the fold result is being applied to z0.
  where c x k z = f z x >>= k

-- https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.Foldable.html#foldrM
-- I guess for symmetry they define `foldrM` with respect to `foldl`? Not
-- sure why this doesn't directly use `foldr`?
foldrM f z0 xs = foldl c return xs z0
  where c k x z = f x z >>= k
```

## Sources

* This was useful: https://wiki.haskell.org/Foldr_Foldl_Foldl'
