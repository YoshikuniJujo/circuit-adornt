{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Control.Monad.State
import Data.Maybe
import Data.Map

getModify :: MonadState m =>
	(StateType m -> a) -> (StateType m -> StateType m) -> m a
getModify g m = gets g <* modify m

indexOrEmpty :: Ord k => Map k [v] -> k -> [v]
indexOrEmpty = (fromMaybe [] .) . (!?)

(!!?) :: Ord k => Map k [v] -> k -> Maybe v
m !!? k = join $ listToMaybe <$> m !? k

insertPush :: Ord k => k -> v -> Map k [v] -> Map k [v]
insertPush k v m = insert k (init vs ++ [v]) m
	where vs = fromMaybe [] $ m !? k
