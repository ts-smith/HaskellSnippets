import Data.Monoid

newtype Stupid a = Stupid { unstupid :: a } deriving (Show)

instance Monad Stupid where
    return a = Stupid a

    s >>= f = let a = unstupid s
                  mb = f a
                  mc = f a
              in mc

sugar = do
    a <- Stupid 1
    b <- Stupid a
    c <- Stupid 2
    return c

sugarfree = Stupid 1 >>= (\a ->
            Stupid a >>= (\b -> 
            Stupid 2 >>= (\c ->
            return c)))