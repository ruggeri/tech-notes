{-# LANGUAGE RankNTypes #-}

id :: forall a. a -> a
id x = x

type IdFunc = forall a. a -> a
id2 :: IdFunc
id2 x = x
