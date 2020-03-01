module UnfoldSample where


myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f x = case f x of
    Nothing     -> []
    Just (a, b) -> a : unfoldr' f b
