module ListSample where

import           Data.List

xs =
    [ (1, 1)
    , (1, 2)
    , (1, 4)
    , (1, 2)
    , (2, 10)
    , (2, 30)
    , (3, 400)
    , (3, 200)
    , (3, 100)
    ]


grouped = groupBy (\a b -> fst a == fst b) xs

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f xs = groupBy'' f xs []

groupBy'' :: (a -> a -> Bool) -> [a] -> [[a]] -> [[a]]
groupBy'' _ []       xss = xss
groupBy'' f (x : xs) xss = groupBy'' f xs $ appendBy f x [] xss

appendBy :: (a -> a -> Bool) -> a -> [[a]] -> [[a]] -> [[a]]
appendBy f x yss []         = yss ++ [[x]]
appendBy f x yss (xs : xss) = if f x (head xs)
    then yss ++ [xs ++ [x]] ++ xss
    else appendBy f x (yss ++ [xs]) xss
