{-# LANGUAGE GADTs, DataKinds, RankNTypes #-}

module ECL.Functions where

import ECL.DataTypes

vecToList :: Vec n a -> [a]
vecToList Nil = []
vecToList (Cons x xs) = x : vecToList xs

listToVec :: (forall n. Vec n a -> r) -> [a] -> r
listToVec f [] = f Nil
listToVec f (x:xs) = listToVec (f . Cons x) xs

unzipVec :: Vec n (a, b) -> (Vec n a, Vec n b)
unzipVec Nil = (Nil, Nil)
unzipVec (Cons (x, y) rest) = (Cons x xs, Cons y ys) where (xs, ys) = unzipVec rest

