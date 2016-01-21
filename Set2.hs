{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where 
        show (Just a) = "Just " ++ show a
        show Nothing = "Nothing"

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay::[a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs 

lookupMay :: Eq a => a->[(a,b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay a ((b,v):xs) 
    | a == b =  Just v 
    | otherwise = lookupMay a xs

divMay::(Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay a b = Just $ a/b

maximumMay::Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = Just $ foldl maxm x xs 
maxm::Ord a => a -> a -> a
maxm x y 
    | x > y = x
    | otherwise = y

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d key = case lookupMay key d of 
                Nothing -> Nothing
                Just xs -> case tailMay xs of
                    Nothing -> Nothing
                    Just t -> case maximumMay t of 
                        Nothing -> Nothing
                        Just m -> case headMay xs of
                            Nothing -> Nothing
                            Just h -> divMay (fromInteger m) (fromInteger h)
