{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude

newtype Gen s a = Gen { run:: s -> (a, s)}

class Monad m where 
    bind::m a -> (a -> m b) -> m b
    return:: a-> m a 

instance Monad (Gen s) where 
    return a = Gen (\ s -> (a,s))
    bind ma f = Gen $ \s -> let (a', s') = run ma s 
                                 in run (f a') s'
-- Rewritten from ealier
-- repRandom
--sequence:: Monad m => [m a] -> m [a]
--sequence mas = return $ foldl (\ as ma -> bind ma (:as)) [] mas 

-- chain
(=<<):: Monad m => (a -> m b) -> m a -> m b
(=<<) f ma = bind ma f 

-- ylink/gernalB/allPerms
liftM2:: Monad m => (a->b->c)-> m a -> m b -> m c 
liftM2 f ma mb = bind ma (\ a -> bind mb  (return . f a) )

liftM3:: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc =  return f `ap` ma `ap` mb `ap` mc  

-- permStep
ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = bind mf (\f -> bind ma (return . f))

-- combine, but I didn't have a combine
join :: Monad m => m (m a) -> m a
join m = bind m id 


