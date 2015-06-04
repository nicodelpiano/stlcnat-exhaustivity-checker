module ECL.ClassicChecker where

import ECL.DataTypes

-- | Type Synonyms for pattern
--   * representation
--   *

--   * Basic representations of cases
type Case = [Binder]
type Cases = [Case]

--   * Result
type Result = Cases

--   * Pattern Matching definition 
type DefPm = Cases

-- | unmatched: returns the list
--   * of `_` for the first uncovered set
unmatched :: Case -> Cases
unmatched c = [unmatched' c]

unmatched' :: Case -> Case
unmatched' = foldr op []
  where
  op :: Binder -> Case -> Case
  op _ c = NullBinder:c

-- | check: given a pattern matching definition
--   * we check whether or not it is exhaustive
--   * giving the resultant non-covered cases
ccheck :: DefPm -> Result
ccheck [] = []
ccheck pmdef@(pm:_) =
  check_uncovers unm pmdef
  where
  unm = unmatched pm

-- | uncover: single binder uncovering
--   * this returns the difference between
--   * two single binders
uncover :: Binder -> Binder -> Case
uncover Zero Zero = []
uncover _ NullBinder = []
uncover NullBinder b =
  concat $ map (\cp -> uncover cp b) all_pat
  where
  all_pat = [Zero, Succ NullBinder]
uncover (Succ n) (Succ m) =
  map Succ ucs
  where
  ucs = uncover n m
uncover b _ = [b]

-- | uncovers: given an uncovered case and a clause
--   * of a pattern match definition,
--   * returns the uncovered cases
uncovers :: Case -> Case -> Cases
-- empty case
uncovers [] [] = []
-- any-var
uncovers (u:us) (NullBinder:ps) =
  map (u:) (uncovers us ps)
-- zero-zero
uncovers (Zero:us) (Zero:ps) =
  map (Zero:) (uncovers us ps)
-- succ-zero
uncovers ucs@((Succ _):_) (Zero:_) =
  [ucs]
-- zero-succ
uncovers ucs@(Zero:_) ((Succ _):_) =
  [ucs]
-- succ-succ
uncovers ((Succ n):us) ((Succ m):ps) =
  let ucs = uncovers (n:us) (m:ps)
  in map (zip_con Succ) ucs
    where
    -- This *must* be generalized for all constructors
    zip_con :: (Binder -> Binder) -> Case -> Case
    zip_con _ [] = []
    zip_con b xs = (b hps):rest
      where
      (hps, rest) = (head xs, tail xs)
-- var-nat
uncovers (NullBinder:us) pats@(_:_) =
  -- This must be something that traverse all constructors of a type
  let all_pat = [Zero, Succ NullBinder]
      uncovered = map (\cp -> uncovers (cp:us) pats) all_pat
  in concat uncovered
-- I guess other cases must fail inside a monad
uncovers _ _ = []

-- | uncovering: returns the cases
--   * that are not covered by a single clause
uncovering :: Cases -> Case -> Cases
uncovering unc clause =
  concat $ map (\u -> uncovers u clause) unc

-- | check_uncovers: Given an uncovered set and a full pm definition
--   * this calculates the cases that are not covered by
--   * the pattern matching definition
check_uncovers :: Cases -> Cases -> Cases
check_uncovers ucs [] = ucs
check_uncovers ucs (c:cs) =
  let ucs' = uncovering ucs c
  in check_uncovers ucs' cs

-- | is_exhaustive: returns whether or not
--   * the given definition is exhaustive
is_exhaustive :: DefPm -> Bool
is_exhaustive = null . ccheck
