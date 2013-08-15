import Control.Monad.Trans.Class
--import Control.Monad.Trans.List

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = do 
	x <- mx
	return $ f x


test = do
	b <- liftM (+2) [1,2,3]
	return (b * 2)

newtype ListT m a = ListT { runListT :: m [a] }

instance (Monad m) => Monad (ListT m) where
	tm >>= fmb = ListT $ runListT tm
					>>= \xs -> mapM (runListT . fmb) xs
						>>= \yss -> return (concat yss)

instance MonadTrans ListT where
	lift m = ListT (m >>= return [m])