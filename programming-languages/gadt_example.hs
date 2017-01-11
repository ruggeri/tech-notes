{-#LANGUAGE GADTs #-}

-- Source: https://en.wikibooks.org/wiki/Haskell/GADT
-- Look, a type-safe expression library using GADT. You can't do this
-- without GADT, because eval needs to know what kind of expression
-- you're talking about.

data Expression a where
  I :: Int -> Expression Int
  B :: Bool -> Expression Bool
  Add :: Expression Int -> Expression Int -> Expression Int
  -- Notice this type constraint!
  Eq :: Eq a => Expression a -> Expression a -> Expression Bool

eval :: Expression a -> a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Eq e1 e2) = (eval e1) == (eval e2)

-- Here are some half ass ways:

-- BS! Can't enforce that expressions being added are of correct type!
data Expression2 = I2 Int
                 | B2 Bool
                 | Add2 Expression2 Expression2
                 | Eq2 Expression2 Expression2

-- The intent here is that when you use I Int, you explicitly "mark"
-- it as an instance of Expression3 Int (you'd have to most likely,
-- because it's ambiguous otherwise!).
--
-- You can address that by writing a functions like i :: Int ->
-- Expression3 Int.
data Expression3 a = I3 Int
                   | B3 Bool
                   | Add3 (Expression3 Int) (Expression3 Int)
                   | Eq3 (Expression3 a) (Expression3 a)

-- But you're still going to have problems writing eval. Consider the
-- very first case: eval (I n) = n. Remember that eval :: Expression3
-- a -> a. But Haskell doesn't know that if we match (I n), that means
-- that a = Int. Remember, we can mistag I n as an Expression3 Bool.

{-#LANGUAGE EmptyDataDecls #-}

-- More! Let's use GADT to implement a safe version of the `head`
-- method!

data EmptyTag
data NonEmptyTag

data SafeList a b where
  Empty :: SafeList a EmptyTag
  Cons :: a -> SafeList a b -> SafeList a NonEmptyTag

myList = (Cons 1 (Cons 2 (Empty)))

safeHead :: SafeList a NonEmptyTag -> a
safeHead (Cons a rest) = a

-- TODO: Not clear how to make a safeTail. Could be tricky!

-- GADT subsumes Existential types!

data Showable where
  MkShowable :: Show a => a -> Showable

myList2 :: [Showable]
myList2 = [(MkShowable 1), (MkShowable "abc"), (MkShowable (7, 5))]

showShow :: Showable -> String
showShow (MkShowable a) = show a

myList3 :: [String]
myList3 = map showShow myList2

-- SOURCE: https://en.wikibooks.org/wiki/Haskell/GADT
