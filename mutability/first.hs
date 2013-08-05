import Control.Monad.ST
import Data.STRef
import Data.Map(Map)
import qualified Data.Map as M
import Data.Monoid(Monoid(..))

memo :: (Ord a) => (a -> b) -> ST s (a -> ST s b)
memo f = do m <- newMemo
            return (withMemo m f)

newtype Memo s a b = Memo (STRef s (Map a b))

newMemo :: (Ord a) => ST s (Memo s a b)
newMemo = Memo `fmap` newSTRef mempty

withMemo :: (Ord a) => Memo s a b -> (a -> b) -> (a -> ST s b)
withMemo (Memo r) f a = do m <- readSTRef r
                           case M.lookup a m of
                              Just b -> return b
                              Nothing -> do let b = f a
                                            write STRef r (M.insert a b m)
                                            return b
