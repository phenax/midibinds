module Utils where

import Data.Function (on)

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

divF :: Int -> Int -> Float
divF = (/) `on` fromIntegral

toPerc :: Int -> Int
toPerc n = (100 * n) `div` 127
