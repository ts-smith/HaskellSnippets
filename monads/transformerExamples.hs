import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Identity
import Control.Monad.IO.Class

test3 = do
    modify (+ 1)
    lift $ modify (++ "1")
    a <- get
    b <- lift get
    return (a,b)

test5 = do
    modify (+ 1)
    a <- get
    lift (print a)
    modify (+ 1)
    b <- get
    lift (print b)
    return ()

test7 :: StateT Integer (StateT String IO) (Integer,String)
test7 = do
    modify (+ 1)
    lift $ modify (++ "1")
    liftIO $ putStrLn "Fuck"
    a <- get
    b <- lift get
    return (a,b)

go7 = evalStateT (evalStateT test7 0) "0"    

go5 = evalStateT test5 0

go3 = runIdentity $ evalStateT (evalStateT test3 0) "0"