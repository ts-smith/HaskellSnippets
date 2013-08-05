import Control.Monad.ST
import Data.STRef
import Control.Monad

--The function takes in a regular list, and returns a regular number
sumST :: Num a => [a] -> a
sumST xs = runST $ do       --runST takes stateful code and makes it pure
   n <- newSTRef 0          --create a Stateful reference to store values
   forM_ xs $ \x -> do      --for each element of xs do the lambda...
      modifySTRef n (+x)    

   readSTRef n

foldlST :: (a -> b -> a) -> a -> [b] -> a
foldlST f acc xs = runST $ do --pull the state out
   acc' <- newSTRef acc --create a statefull accumulator

   forM_ xs $ \x -> do
      a <- readSTRef acc'
      writeSTRef acc' (f a x) --apply f to the accumulator and x

   readSTRef acc'

fibST :: Integer -> Integer
fibST n = 
   if n < 2
   then n
   else runST $ do
      x <- newSTRef 0
      y <- newSTRef 1
      fibST' n x y

   where fibST' 0 x _ = readSTRef x
         fibST' n x y = do
            x' <- readSTRef x
            y' <- readSTRef y
            writeSTRef x y'
            writeSTRef y $! x'+y' --strictly evaluate the function
            fibST' (n-1) x y

main = do print $ sumST [1,2,3,4]
          print $ foldlST (+) 0 $ take 999 [1..]
          return $ fibST 160
