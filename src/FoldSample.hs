module FoldSample where

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> f a : b) []

foldLefts :: [Either a b] -> [a]
foldLefts = foldr (\a b -> left2List a ++ b) []

left2List :: Either a b -> [a]
left2List (Left x) = [x]
left2List _        = []

foldRights :: [Either a b] -> [b]
foldRights = foldr (\a b -> right2List a ++ b) []

right2List :: Either a b -> [b]
right2List (Right x) = [x]
right2List _         = []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = partitionEithers'' xs ([], [])

partitionEithers'' :: [Either a b] -> ([a], [b]) -> ([a], [b])
partitionEithers'' []             t        = t
partitionEithers'' (Left  a : xs) (ys, ws) = partitionEithers'' xs (a : ys, ws)
partitionEithers'' (Right b : xs) (ys, ws) = partitionEithers'' xs (ys, b : ws)
