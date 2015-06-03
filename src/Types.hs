{-# LANGUAGE GADTs, DataKinds, RankNTypes #-}

module Types where

import AST
import Match

data Nat = Z | S Nat

data Vec n a where
  Nil :: Vec Z a
  Cons :: a -> Vec n a -> Vec (S n) a

vecToList :: Vec n a -> [a]
vecToList Nil = []
vecToList (Cons x xs) = x : vecToList xs

listToVec :: (forall n. Vec n a -> r) -> [a] -> r
listToVec f [] = f Nil
listToVec f (x:xs) = listToVec (f . Cons x) xs

unzipVec :: Vec n (a, b) -> (Vec n a, Vec n b)
unzipVec Nil = (Nil, Nil)
unzipVec (Cons (x, y) rest) = (Cons x xs, Cons y ys) where (xs, ys) = unzipVec rest

instance (Show a) => Show (Vec n a) where
  show Nil = "Nil"
  show (Cons x xs) = "(Cons " ++ show x ++ " " ++ show xs ++ ")"

instance Match Binder where
  missed _ NullBinder = []

  missed NullBinder b = go b
    where 
    go Zero = [Succ NullBinder]
    go (Succ n) = Zero : map Succ (go n)
    go _ = []

  missed Zero Zero = []
  missed (Succ n) (Succ m) = map Succ (missed n m)

  missed b _ = [b]

instance (Match c) => Match (Vec n c) where
  missed Nil _ = []
  missed (Cons x xs) (Cons y ys) | null miss = map (Cons x) (missed xs ys)
                                 | otherwise = map (`Cons` xs) miss ++ map (Cons y) (missed xs ys)
    where
    miss = missed x y

{- instance (Match c) => Match (Vec n c) where
  missed Nil Nil = []
  missed (Cons x xs) (Cons y ys) = map (`Cons` xs) (missed x y) ++ map (Cons x) (missed xs ys) -}
