module Match where

class Match c where
  full :: c
  -- | missed: single binder uncovering
  --   * this returns the difference between
  --   * two single binders
  missed :: c -> c -> [c]
