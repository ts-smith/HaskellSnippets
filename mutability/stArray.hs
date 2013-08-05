import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import Data.Array
--http://www.haskell.org/haskellwiki/Library/ArrayRef#Reimplemented_Arrays_library
--look at that for dynamically resizable arrays

buildPair = do arr <- newArray (0,9) 37 :: ST s (STArray s Int Int)
               a <- readArray arr 1
               writeArray arr 1 64
               b <- readArray arr 1
               return (a,b)

main = do
   print $ runST buildPair
   print $ runSTArray stmonad
   print $ runSTArray $ thawArray $ array (0,9) [(i,i) | i <- [0..9]]

stmonad = do arr <- newArray (0,9) 0 :: ST s (STArray s Int Int)
             writeArray arr 0 42
             return arr

thawArray arr = do
   --to take advantage of unsafeThaw, must compile with -O flag.
   --also, must be a Array to STArray or UArray to STUArray, basically
   thawed <- unsafeThaw arr
   a <- readArray thawed 1
   writeArray thawed 1 (a+2)
   return thawed
