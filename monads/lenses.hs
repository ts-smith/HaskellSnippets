{-# LANGUAGE TemplateHaskell #-}

--http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html
import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Control.Monad

data Game = Game
    { _score :: Int
    , _units :: [Unit]
    , _boss  :: Unit
    } deriving (Show)

data Unit = Unit
    { _health   :: Int
    , _position :: Point
    } deriving (Show)

data Point = Point
    { _x :: Double
    , _y :: Double
    } deriving (Show)

makeLenses ''Game
makeLenses ''Unit
makeLenses ''Point


initialState :: Game
initialState = Game
    { _score = 0
    , _units =
        [ Unit
            { _health = 10
            , _position = Point { _x = 3.5, _y = 7.0 }
            }
        , Unit
            { _health = 15
            , _position = Point { _x = 1.0, _y = 1.0 }
            }
        , Unit
            { _health = 8
            , _position = Point { _x = 0.0, _y = 2.1 }
            }
        ]
    , _boss = Unit
        { _health = 100
        , _position = Point { _x = 0.0, _y = 0.0 }
        }
    }

--execStateT strike initialState
--this type declaration means a State transformer with an IO interior monadTransformer and No output from the state process
strike :: StateT Game IO ()
strike = do
    lift $ putStrLn "*shink*"
    boss.health -= 10
    --lift $ putStrLn $ show (boss.health)

    --newState <- execStateT strike initialState
    --newState^.boss.health

around :: Point -> Double -> Traversal' Unit Unit
around center radius = filtered (\unit ->
    (unit^.position.x - center^.x)^2
  + (unit^.position.y - center^.y)^2
  < radius^2 )

fireBreath :: Point -> StateT Game IO ()
fireBreath target = do
    lift $ putStrLn "*rawr*"
    units.traversed.(around target 1.0).health -= 3

partyLoc :: Traversal' Game Point
partyLoc = units.traversed.position

retreat :: StateT Game IO ()
retreat = do
    lift $ putStrLn "Retreat!"
    zoom partyLoc $ do
        x += 10
        y += 10


battle :: StateT Game IO ()
battle = do
    -- Charge!
    forM_ ["Take that!", "and that!", "and that!"] $ \taunt -> do
        lift $ putStrLn taunt
        strike

    -- The dragon awakes!
    fireBreath (Point 0.5 1.5)
    
    replicateM_ 3 $ do
        -- The better part of valor
        retreat

        -- Boss chases them
        zoom (boss.position) $ do
            x += 10
            y += 10