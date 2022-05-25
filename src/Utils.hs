module Utils where

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x
