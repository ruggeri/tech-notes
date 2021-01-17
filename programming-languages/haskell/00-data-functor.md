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
