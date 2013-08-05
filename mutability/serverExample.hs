import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar
import qualified Data.HashMap.Strict as H
import Data.Foldable (forM_)

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (unless)
import Control.Monad.Trans.State.Strict
import qualified Data.ByteString.Char8 as B
import Control.Proxy
import Control.Proxy.TCP
import System.IO

main = do
    hSetBuffering stdout NoBuffering

    {- These are the internal data structures.  They should be an implementation
       detail and you should never expose these references to the
       "business logic" part of the application. -}
    -- I use nRef to keep track of creating fresh Ints (which identify users)
    nRef <- newTVarIO 0       :: IO (TVar Int)
    {- hMap associates every user (i.e. Int) with a counter

       Notice how I've "striped" the hash map by storing STM references to the
       values instead of storing the values directly.  This means that I only
       actually write the hashmap when adding or removing users, which reduces
       contention for the hash map.

       Since each user gets their own unique STM reference for their counter,
       modifying counters does not cause contention with other counters or
       contention with the hash map. -}
    hMap <- newTVarIO H.empty :: IO (TVar (H.HashMap Int (TVar Int)))

    {- The following code makes heavy use of Haskell's pure closures.  Each
       'let' binding closes over its current environment, which is safe since
        Haskell is pure. -}

    let {- 'getCounters' is the only server-facing command in our STM API.  The
           only permitted operation is retrieving the current set of user
           counters.

           'getCounters' closes over the 'hMap' reference currently in scope so
           that the server never needs to be aware about our internal
           implementation. -}
        getCounters :: STM [Int]
        getCounters = do
            refs <- fmap H.elems (readTVar hMap)
            mapM readTVar refs

        {- 'init' is the only client-facing command in our STM API.  It
            initializes the client's entry in the hash map and returns two
            commands: the first command is what the client calls to 'increment'
            their counter and the second command is what the client calls to log
            off and delete
            'delete' command.

            Notice that those two returned commands each close over the client's
            unique STM reference so the client never needs to be aware of how
            exactly 'init' is implemented under the hood. -}
        init :: STM (STM (), STM ())
        init = do
            n   <- readTVar nRef
            writeTVar nRef $! n + 1

            ref <- newTVar 0
            modifyTVar' hMap (H.insert n ref)

            let incrementRef :: STM ()
                incrementRef = do
                    mRef <- fmap (H.lookup n) (readTVar hMap)
                    --mRef has type Maybe (TVar Int)
                    --forM_ conveniently takes the value out and runs the result of the lambda
                    forM_ mRef $ \ref -> modifyTVar' ref (+ 1)

                deleteRef :: STM ()
                deleteRef = modifyTVar' hMap (H.delete n)

            return (incrementRef, deleteRef)

    {- Now for the actual program logic.  Everything past this point only uses
       the approved STM API (i.e. 'getCounters' and 'init').  If I wanted I
       could factor the above approved STM API into a separate module to enforce
       the encapsulation boundary, but I am lazy. -}

    {- Fork a thread which polls the current state of the counters and displays
       it to the console.  There is a way to implement this without polling but
       this gets the job done for now.

       Most of what it is doing is just some simple tricks to reuse the same
       console line instead of outputting a stream of lines.  Otherwise it
       would be just:

       forkIO $ forever $ do
           ns <- atomically getCounters
           print ns
    -}
    forkIO $ (`evalStateT` 0) $ forever $ do
        del <- get
        lift $ do
            putStr (replicate del '\b')
            putStr (replicate del ' ' )
            putStr (replicate del '\b')
        ns <- lift $ atomically getCounters
        let str = show ns
        lift $ putStr str
        put $! length str
        lift $ threadDelay 10000

    {- Fork a thread for each incoming connection, which listens to the client's
       commands and translates them into 'STM' actions -}
    serve HostAny "8080" $ \(socket, _) -> do
        (increment, delete) <- atomically init

        {- Right now, just do the dumb thing and convert all keypresses into
           increment commands, with the exception of the 'q' key, which will
           quit -}
        let handler :: (Proxy p) => () -> Consumer p Char IO ()
            handler () = runIdentityP loop
              where
                loop = do
                    c <- request ()
                    unless (c == 'q') $ do
                        lift $ atomically increment
                        loop

        {- This uses my 'pipes' library.  It basically is a high-level way to
           say:

           * Read binary packets from the socket no bigger than 4096 bytes

           * Get the first character from each packet and discard the rest

           * Handle the character using the above 'handler' function -}
        runProxy $ socketReadS 4096 socket >-> mapD B.head >-> handler

        {- The above pipeline finishes either when the socket closes or
           'handler' stops looping because it received a 'q'.  Either case means
           that the client is done so we log them out using 'delete'. -}
        atomically delete
