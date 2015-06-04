{-# LANGUAGE GADTs, DataKinds, RankNTypes #-}

module ECL.Checker where

import ECL.Match
import ECL.DataTypes
import ECL.Instances
import ECL.Functions

check :: (Match c) => [(c, c)] -> [[c]]
check = listToVec (map vecToList . uncurry missed . unzipVec)
