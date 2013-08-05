import Data.IORef
import Control.Monad

data Point = XY { x:: IORef Int, y :: IORef Int }

newPoint (x,y) = do xRef <- newIORef x
                    yRef <- newIORef y
                    return (XY xRef yRef)

movePoint (deltaX, deltaY) (XY x y) = do modifyIORef x (+ deltaX)
                                         modifyIORef y (+ deltaY)

printPoint (XY x y) = do x' <- readIORef x
                         y' <- readIORef y
                         print (x', y')

main = do p <- newPoint (1,2)
          printPoint p
          movePoint (3,2) p
          printPoint p

          ref <- newIORef 0
          replicateM_ 1000000 $ modifyIORef ref (+1)
          readIORef ref >>= print
