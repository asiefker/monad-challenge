{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs = allPerms (,) 

data Card = Card Int String
instance Show Card where 
        show (Card r s) = show r ++ s

allCards :: [Int] -> [String] -> [Card]
allCards  = allPerms Card  

allPerms :: (a -> b -> c) -> [a] -> [b] -> [c]
allPerms f as  = permStep (permStep [f] as)    

allPerms3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allPerms3 f as bs  = permStep (permStep (permStep [f] as) bs)

permStep :: [a -> b] -> [a] -> [b]
permStep [] _ = []
permStep (f:fs) as = map f as ++ permStep fs as 

