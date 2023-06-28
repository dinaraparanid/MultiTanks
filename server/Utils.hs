module Utils where

import qualified Data.Maybe as Maybe 

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:xs) = Just x