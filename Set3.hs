{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs [] _ =  []
allPairs (x:xs) bs = _allPairs x bs ++ allPairs xs bs 

_allPairs :: a -> [b] -> [(a,b)]
_allPairs _ [] = []
_allPairs a (x:xs) = (a,x) : _allPairs a xs 

data Card = Card Int String


instance Show Card where 
        show (Card r s) = show r ++ s

allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards (x:rs) bs = _allCards x bs ++ allCards rs bs

_allCards :: Int -> [String] -> [Card]
_allCards _ [] = []
_allCards r (s:ss) = Card r s : _allCards r ss
