{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

fiveRands::[Integer]
fiveRands=recur 1 (mkSeed 1)

recur 6 _ = []
recur x seed = let r = Set1.rand seed
                    in fst r : recur (x+1) (snd r) 
-- randEven::Gen Integer
-- randEven s = rand :e * 2 
rand :: Gen Integer
rand  = MCPrelude.rand  

randLetter:: Gen Char
randLetter = generalA toLetter 

randString3::String
randString3 = [l1, l2, l3]
    where
        (l1, s1) = randLetter (mkSeed 1)
        (l2, s2) = randLetter s1
        (l3, _) = randLetter s2

type Gen a = Seed -> (a, Seed)
                     
randEven :: Gen Integer
randEven = generalA (*2) 

randOdd :: Gen Integer
randOdd= generalA (+1) 

randTen :: Gen Integer
randTen = generalA (*10) 

generalA::(Integer->a)-> Gen a
generalA f s = ( f i, s') 
    where(i,s') =  Set1.rand s

randPair::Gen (Char, Integer)
randPair = generalPair2 randLetter Set1.rand

generalPair::Gen a -> Gen b -> Gen (a,b)
generalPair ga gb s =  let  (a,s') = ga s
                            (b,s'') = gb s'
                        in ((a,b), s'')

generalB:: (a->b->c) -> Gen a -> Gen b -> Gen c
generalB f ga gb s = let  (a,s') = ga s
                          (b,s'') = gb s'
                        in (f a b, s'')

generalPair2::Gen a -> Gen b -> Gen (a,b)
generalPair2 = generalB (,) 

repRandom :: [Gen a] -> Gen [a]
repRandom  as s = foldl (\b a-> let (a', s') = a (snd b) in (fst b ++ [a'],s')) (mkGen [] s) as

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo a f s = let (a', s') = a s 
                  in f a' s'

mkGen :: a -> Gen a
mkGen a s = (a,s)
