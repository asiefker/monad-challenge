{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude
import Set4

data Maybe a = Just a | Nothing

instance Monad Maybe  where  
    return = Just 
    bind Nothing _ = Nothing
    bind (Just a) f= f a

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

chain:: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain f (Just a)= f a

link :: Maybe a -> (a-> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 d key = let t = maximumMay =<< (tailMay =<< lookupMay key d )
                        h = headMay =<< lookupMay key d 
                    in (\ ta -> (divMay (fromInteger ta) . fromInteger) =<< h )=<< t

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries d n1 n2 = link (lookupMay n1 d) (\s1 -> link (lookupMay n2 d) (\s2 -> Just $ s1 + s2) )

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 d n1 n2 = liftM2 (+) (lookupMay n1 d) (lookupMay n2 d)
