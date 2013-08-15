import Control.Monad.Trans.Class
import Control.Monad.Trans.State


type TheState = Int
type Output = Int

fuck :: StateT TheState IO Output
fuck = do
	a <- state $ \st -> (st + 3, st + 3)
	b <- state $ \st -> (st + a, st)
	c <- get
	lift $ putStrLn "Fuck!"
	return (b + c)

--runStateT fuck 2