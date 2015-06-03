{-# LANGUAGE GADTs, DataKinds, RankNTypes #-}

module Checker where

import Match
import DataTypes
import Instances
import Functions

check :: (Match c) => [(c, c)] -> [[c]]
check = listToVec (map vecToList . uncurry missed . unzipVec)
