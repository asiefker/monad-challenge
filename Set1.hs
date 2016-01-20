{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

fiveRands::[Integer]
fiveRands=recur 1 (mkSeed 1)

recur 6 _ = []
recur x seed = let r = rand seed
                    in fst r : recur (x+1) (snd r) 

randLetter::Seed -> (Char, Seed)
randLetter seed = let (i,s) = rand seed
                   in (toLetter i, s)

randString3::String
randString3 = recurL 1 (mkSeed 1) 

recurL 4 _ = ""
recurL x seed = let (l,s)  = randLetter seed
                    in l : recurL (x+1) s

type Gen a = Seed -> (a, Seed)
randG :: Gen Integer
randG s = (1, mkSeed 1)
