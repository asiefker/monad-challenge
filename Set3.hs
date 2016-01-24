{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude
import Set4

instance Monad [] where 
    return a = [a]
    bind as f = foldr ((++).f) [] as 

allPairs :: [a] -> [b] -> [(a,b)]
allPairs = liftM2 (,) 

data Card = Card Int String
instance Show Card where 
        show (Card r s) = show r ++ s

allCards :: [Int] -> [String] -> [Card]
allCards  = liftM2 Card  

allPerms :: (a -> b -> c) -> [a] -> [b] -> [c]
allPerms f as  = permStep (permStep [f] as)    

allPerms3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allPerms3 f as bs  = permStep (permStep (permStep [f] as) bs)

permStep :: [a -> b] -> [a] -> [b]
permStep [] _ = []
permStep (f:fs) as = map f as ++ permStep fs as 

