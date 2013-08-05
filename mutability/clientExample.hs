import Control.Monad
import Control.Proxy
import Control.Proxy.Safe
import Control.Proxy.TCP.Safe
import Data.ByteString.Char8 (pack)
import System.IO

main = do
    hSetBuffering stdin NoBuffering
    hSetEcho      stdin False

    {- Again, this uses my 'pipes' library.  It basically says:

        * Read characters from the console using 'commands'

        * Pack them into a binary format

        * send them to a server running at 127.0.0.1:8080

        This finishes looping when the user types a 'q' or the connection is
        closed for whatever reason.
    -}
    runSafeIO $ runProxy $ runEitherK $
         try . commands
     >-> mapD (\c -> pack [c])
     >-> connectWriteD Nothing "127.0.0.1" "8080"

commands :: (Proxy p) => () -> Producer p Char IO ()
commands () = runIdentityP loop
  where
    loop = do
        c <- lift getChar
        respond c
        unless (c == 'q') loop
