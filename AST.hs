module AST where

-- | Name binding association
data Binder = NullBinder       -- Wildcard binder
            | Zero             -- Zero binder
            | Succ Binder      -- Nat binder
  deriving (Show, Eq)

-- | Basic representations of cases
type Case = [Binder]

type Cases = [Case]

-- | Result
type Result = Cases

-- | Pattern Matching definition 
type DefPm = Cases
