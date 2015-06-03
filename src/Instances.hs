{-# LANGUAGE GADTs, DataKinds, RankNTypes #-}

module Instances where

import Match
import DataTypes

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
