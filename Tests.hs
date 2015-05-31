module Tests where

import AST
import Checker
import Data.List

type TestCase = Cases

n = NullBinder

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
exh = [[Succ n], [Zero]]

-- | min: the patterns of 
--   * a `min` function
_min :: DefPm
_min = [[Zero, n], [n, Zero], [Succ n, Succ n]]

-- | crazy: some crazy function
crazy :: DefPm
crazy = [[Zero, Succ n, Succ Zero], [Succ n, Zero, Succ $ Succ Zero]]

-- | Checking the results of the above definitions
--   * The uncovered cases for each definition were taken by executing
--   * those definitions in Haskell with -Wall

-- * f : should not be exhaustive
-- * the missing case is [Succ _]
test_f :: Bool
test_f = test f [[Succ n]]

-- * g: should not be exhaustive
-- * the missing cases are [Zero, Succ Zero, Succ (Succ Zero), Succ (Succ (Succ (Succ _)))]
test_g :: Bool
test_g = test g [[Zero], [Succ Zero], [Succ (Succ Zero)], [Succ (Succ (Succ (Succ n)))]]

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
test_crazy = test crazy [[Zero, Zero, n], [Zero, Succ n, Zero], [Zero, Succ n, Succ $ Succ n], [Succ n, Succ n, n],
                         [Succ n, Zero, Zero], [Succ n, Zero, Succ Zero], [Succ n, Zero, Succ $ Succ $ Succ n]]
