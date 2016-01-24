{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude

newtype Gen s a = Gen { run:: s -> (a, s)}

data Maybe a = Just a | Nothing

class Monad m where 
    bind::m a -> (a -> m b) -> m b
    return:: a-> m a 

instance Monad [] where 
    return a = [a]
    bind as f = foldr ((++).f) [] as 

instance Monad Maybe  where  
    return = Just 
    bind Nothing _ = Nothing
    bind (Just a) f= f a

instance Monad (Gen s) where 
    return a = Gen (\ s -> (a,s))
    bind ma f = Gen $ \s -> let (a', s') = run ma s 
                                 in run (f a') s'

ylink:: Monad m => (a->b->c)-> m a -> m b -> m c 
ylink f ma mb = bind ma (\ a -> bind mb  (return . f a) )
