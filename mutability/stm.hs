import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar
import qualified Data.HashMap.Strict as H
import Data.Foldable (forM_)

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (unless)
import Control.Monad.Trans.State.Strict
import qualified Data.ByteString.Char8 as B
import System.IO

main = do
   hSetBuffering stdout NoBuffering

   nRef <- newTVarIO 0           :: IO (TVar Int)

   hMap <- newTVarIO H.empty     :: IO (TVar (H.HashMap Int (TVar Int)))

   let getCounters :: STM [Int]
       getCounters = do
         refs <- fmap H.elems (readTVar hMap)
         mapM readTVar refs

       init :: STM (STM (), STM())
       init = do
         n <- readTVar nRef
         writeTVar nRef $! n + 1

         ref <- newTVar 0
         modifyTVar' hMap (H.insert n ref)

         let incrementRef :: STM ()
             incrementRef = do
               mRef <- fmap (H.lookup n) (readTVar hMap)
