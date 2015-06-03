{-# LANGUAGE GADTs, DataKinds, RankNTypes #-}

module Tests where

import Data.List
import Data.Functor
import Test.QuickCheck

import DataTypes
import Checker
import ClassicChecker
import Instances 
import Match

nb :: Binder
nb = NullBinder

-- |
-- | QuickCheck
--   *
--   * Arbitrary binders instance

instance Arbitrary Binder where
  arbitrary = oneof [return NullBinder, return Zero, Succ <$> arbitrary]

compare_missed bl br = uncover bl br == missed bl br

--    * 
--    * Arbitrary Nat instance

instance Arbitrary Nat where
  arbitrary = oneof [return Z, S <$> arbitrary]

--    *
--    * Properties over generated uncovered Vecs to test missed
prop_miss1 bl br = undefined 
