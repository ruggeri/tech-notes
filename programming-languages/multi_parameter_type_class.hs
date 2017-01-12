{-# LANGUAGE MultiParamTypeClasses #-}

data NList e = Nil
             | Cons e (NList e)

class Collection c where
  contains :: Eq e => e -> c e -> Bool
  isEmpty :: c e -> Bool

instance Collection NList where
  --contains :: Int -> NList Int -> Bool
  contains e Nil = False
  contains e1 (Cons e2 rest) = True --(e1 == e2) `or` (contains e1 rest)

  --isEmpty :: NList Int -> Bool
  isEmpty Nil = True
  isEmpty _   = False
