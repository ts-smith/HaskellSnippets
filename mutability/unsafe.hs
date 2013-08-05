import Data.Array
import Data.Array.Base (unsafeAt)
import Control.Monad
import Data.Array.ST

elems arr = [ unsafeAt arr i
               | i <- [0 .. rangeSize(bounds arr)-1] ]
