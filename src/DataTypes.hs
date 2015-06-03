{-# LANGUAGE GADTs, DataKinds, RankNTypes #-}

module DataTypes where

-- | Nat
data Nat = Z | S Nat
  deriving (Show, Eq)

-- | Name binding association
data Binder = NullBinder       -- Wildcard binder
            | Zero             -- Zero binder
            | Succ Binder      -- Nat binder
  deriving (Show, Eq)

-- | Vec
data Vec n a where
  Nil :: Vec Z a
  Cons :: a -> Vec n a -> Vec (S n) a

instance (Show a) => Show (Vec n a) where
  show Nil = "Nil"
  show (Cons x xs) = "(Cons " ++ show x ++ " " ++ show xs ++ ")"
