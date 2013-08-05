import Data.Array
import Data.Char

main = incremental

indexTest = do
   print $ index (1,9) 1 -- 0, 1 is at position 0
   print $ index ((0,0), (2,2)) (1,1) -- 4, (1,1) is at position 4

boundsTest = do
   print $ range (0,4)
   print $ range ((0,0), (2,2))

--arrays are really weird. They take an index range, which can be of any type (Ix a) and creates and array over that range. That means that an array can have a range of (7,11) or (Pie,Cheese), if they are properly made instances of Ix. When indexing an array, the value is retrieved by this weird index. Each position in the array is an (index,value) pair. So you say I want (thing at index of weird index) to get your value out

--array :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
--array -> creats and array from a (pair of bounds) and a list of (index,value) pairs
simpleArray = do
   let a = array (0,5) [(i,chr i) 
                           | i <- [0..5]]
       b = a!0
   print a
   print b
   print $ bounds a
   
accumulation = do
   --accumArray :: (Ix a) -> (b -> c -> b) -> b -> (a,a) -> [Assoc a c] -> Array a b
   print $ accumArray (+) init bounds list
      where init = 0
            bounds = (0,9)
            list = [(i,1) | i <- indexList]
            indexList = [0..9] ++ [0..4] ++ [0,1] ++ [9] ++ [9] ++ [9]

incremental = do
   let a = array (0,5) [(i, i) | i <- [0..5]]
       modified = a // [(0,50),(1,-20)]
   print modified
