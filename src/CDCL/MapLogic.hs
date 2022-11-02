---------------------------------------------------------------------
-- |
-- Module      :   CDCL.MapLogic
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :   Apache-2.0
-- Maintainer  :
-- Stability   :
-- Portability :
-- = Description
-- Contains functions for changing MappedTupleList.
---------------------------------------------------------------------
module CDCL.MapLogic (pushToMappedTupleList, deleteLvl) where

import           CDCL.Types (Level (..), MappedTupleList, Reason (..), Tuple)

import qualified Data.Map.Strict as Map
import           Data.Maybe

-- | Function for updating MappedTupleList.
--   If Variable was already set return map.
--   If Variable was not set and lvl has no list -> insert the TupelList
--   If Variable was not set but lvl has already a list -> update the TupelList
pushToMappedTupleList :: MappedTupleList -> Level -> Tuple -> Reason -> MappedTupleList
pushToMappedTupleList maptl lvl tupel reason
    | Data.Maybe.isJust f && null check = Map.update m lvl maptl
    | Data.Maybe.isNothing f = Map.insert lvl [(tupel, reason)] maptl
    | otherwise = maptl
    where f = Map.lookup lvl maptl
          check = filter (((== fst tupel) . fst).fst) (fromMaybe [] f)
          m _ = Just (fromMaybe [] f ++ [(tupel, reason)]) -- Not allowed to change "++" to ":", as it will change the decision map

-- | Function removes the given level
deleteLvl :: Level -> MappedTupleList  -> MappedTupleList
deleteLvl = Map.delete
