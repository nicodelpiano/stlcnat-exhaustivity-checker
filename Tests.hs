{-# LANGUAGE GADTs, DataKinds, RankNTypes #-}

module Tests where

import AST
import Checker
import Data.List
import Types
import Match
import Test.QuickCheck

type TestCase = Cases

nb :: Binder
nb = NullBinder

-- | test: Testing function
test :: DefPm -> TestCase -> Bool
test f tc = null $ (\\) (check f) tc

-- | f: simple non-exhaustive
--   * function
f :: DefPm 
f = [[Zero]]

-- | g: another non-exhaustive
--   * function
g :: DefPm
g = [[Succ $ Succ $ Succ Zero]]

-- | exh: simple exhaustive definition
exh :: DefPm
exh = [[Succ nb], [Zero]]

-- | min: the patterns of 
--   * a `min` function
_min :: DefPm
_min = [[Zero, nb], [nb, Zero], [Succ nb, Succ nb]]

-- | crazy: some crazy function
crazy :: DefPm
crazy = [[Zero, Succ nb, Succ Zero], [Succ nb, Zero, Succ $ Succ Zero]]

-- | Checking the results of the above definitions
--   * The uncovered cases for each definition were taken by executing
--   * those definitions in Haskell with -Wall

-- * f : should not be exhaustive
-- * the missing case is [Succ _]
test_f :: Bool
test_f = test f [[Succ nb]]

-- * g: should not be exhaustive
-- * the missing cases are [Zero, Succ Zero, Succ (Succ Zero), Succ (Succ (Succ (Succ _)))]
test_g :: Bool
test_g = test g [[Zero], [Succ Zero], [Succ (Succ Zero)], [Succ (Succ (Succ (Succ nb)))]]

-- * exh: should be exhaustive
test_exh :: Bool
test_exh = test exh []

-- * min: should be exhaustive
test_min :: Bool
test_min = test _min []

-- * crazy: should not be exhaustive
-- * the missing cases are:
-- * *Main> let f Zero (Succ _) (Succ Zero) = Zero; f (Succ _) Zero (Succ (Succ Zero)) = Zero
-- *
-- *Pattern match(es) are non-exhaustive
-- *In an equation for ‘f’:
-- *    Patterns not matched:
-- *        Zero Zero _
-- *        Zero (Succ _) Zero
-- *        Zero (Succ _) (Succ (Succ _))
-- *        (Succ _) (Succ _) _
-- *        (Succ _) Zero Zero
-- *        (Succ _) Zero (Succ Zero)
-- *        (Succ _) Zero (Succ (Succ (Succ _)))
test_crazy :: Bool
test_crazy = test crazy [[Zero, Zero, nb], [Zero, Succ nb, Zero], [Zero, Succ nb, Succ $ Succ nb], [Succ nb, Succ nb, nb],
                         [Succ nb, Zero, Zero], [Succ nb, Zero, Succ Zero], [Succ nb, Zero, Succ $ Succ $ Succ nb]]

-- |
-- | QuickCheck
--   *
--   * Arbitrary binders instance

instance Arbitrary Binder where
  arbitrary = oneof [return NullBinder, return Zero, fmap Succ arbitrary]

compare_missed bl br = uncover bl br == missed bl br

--    * 
--    * Arbitrary Nat instance

instance Arbitrary Nat where
  arbitrary = oneof [return Z, fmap S arbitrary]

--    *
--    * Properties over generated uncovered Vecs to test missed
