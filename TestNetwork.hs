module TestNetwork where

import BoardPrint
import Control.Concurrent
import Control.Exception
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.State
import Helpers
import Logic
import Network.Socket
import System.IO
import System.Random (Random (randomRs), RandomGen, getStdGen)
import Parser

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 4242 0)   -- listen on TCP port 4242.
    listen sock 2                              -- set a max of 2 queued connections
    playerCountVar <- newMVar 0
    mainLoop playerCountVar sock                              -- unimplemented

mainLoop :: MVar Int -> Socket -> IO ()
mainLoop playerCountVar sock = do
    conn <- accept sock     -- accept a connection and handle it
    runConn conn            -- run our server's logic
    mainLoop playerCountVar sock           -- repeat

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode
    let broadcastToAll msg = do 
          hPutStrLn hdl msg
          putStrLn msg
    hSetBuffering hdl NoBuffering

    broadcastToAll "A player has connected!"
    broadcastToAll "~~~~~~~~~~~~~~~\nWelcome to Capture The Mine.\n~~~~~~~~~~~~~~~\n"


    hClose hdl