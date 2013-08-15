import Control.Monad.State

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)  
  
push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs) 

stackManip :: State Stack Int  
stackManip = do  
    push 3  
    a <- pop  
    pop 


l = runState stackManip [1,2,3,4]



type Accumulator = Int

remove :: Int -> State Accumulator Int
remove ammount = state subtractor
    where subtractor accumulator = (new, new)
            where new = accumulator - ammount

add :: Int -> State Accumulator Int
add ammount = state adder
    where adder accumulator = (new,new)
            where new = accumulator + ammount

stateAdd x y = do
    add x
    add y

glue x = do
    a <- stateAdd x 20
    b <- get --pointless
    if b > 20
        then do put 20
                stateAdd 10 20
        else return 0

five = runState (stateAdd 2 3) 0

runit x b = runState (glue x) b

evaluated = evalState (glue 10) 10
executed = execState (glue 0) 0 --end result is 20 from (0,20)

inline = runState (remove 2 >>= (\a -> add a))

--a is the argument the the lambda the f gets called on in bind
--the state passing is also managed by the bind definition
--the reason it always must work is because the type constrains
--there is no way for the things pulled ( a <- ma) not to be the a,
--but with the context applied however it should be