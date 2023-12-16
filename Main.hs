module Main where

import Network.Socket
import Network.BSD (getHostByName, hostAddress)
import System.IO
import Control.Concurrent
import Control.Exception (catch, IOException)
import Control.Monad
import Data.Maybe

import Single
import Multiplayer


main :: IO ()
main = do
    putStrLn $ "~~~~~~~~~~~~~~~\nWelcome to Capture The Mine.\n~~~~~~~~~~~~~~~\n" ++
               "Type 'single' to create a game, 'join' to join an existing game or 'host' to create a new game."
    str <- getLine
    case str of
     "single" -> Single.createGame
     "host" -> hostGame >> main

hostGame :: IO ()
hostGame = withSocketsDo $ do
    -- Create a socket
    sock <- socket AF_INET Stream 0

    -- Set socket options
    setSocketOption sock ReuseAddr 1

    -- Bind the socket to a specific port
    bind sock (SockAddrInet 4242 0)

    -- Listen for incoming connections with a maximum queue length of 5
    listen sock 2

    -- Start communication with the server
    handleConnections sock
    putStrLn "Waiting for players to join..."



-- Function to handle communication with a connected client
handleConnections :: Socket -> IO ()
handleConnections sock = do
    conn <- accept sock     -- accept a connection and handle it
    runConn conn            -- run our server's logic
    handleConnections sock           -- repeat

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    -- Redirect stdout to the handler
    hPutStrLn hdl "Hello, world!"
    setupGameThreads hdl True

    -- Send the output to the handler
    hClose hdl

-- Thread that writes stdin input to the shared channel
getUserInput :: Chan (InputSource, Message) -> IO ()
getUserInput chan = do
    str <- getLine
    writeChan chan (Stdin, str)
    getUserInput chan

-- Thread that writes network messages to the shared channel
getNetworkMsg :: Handle -> Chan (InputSource, Message) -> IO ()
getNetworkMsg handle chan = do
    isHandleClosed <- hIsEOF handle
    if isHandleClosed
    then
        writeChan chan (Network, "exit")
    else do
        msg <- hGetLine handle
        writeChan chan (Network, msg)
        getNetworkMsg handle chan

-- Forks a game thread and returns an MVar that gets filled when the thread exits
forkGameThread :: Handle -> Chan (InputSource, Message) -> ServerFlag -> IO (MVar ())
forkGameThread handle chan isServer = do
    mv <- newEmptyMVar
    _  <- forkFinally (Multiplayer.createGame handle chan isServer) (\_ -> putMVar mv ())
    return mv

setupGameThreads :: Handle -> ServerFlag -> IO ()
setupGameThreads handle isServer = do
    chan   <- newChan
    -- Forks three threads: 1 reads from stdin, 2 reads network msgs, 3 plays the game
    tid1   <- forkIO (getUserInput chan)
    tid2   <- forkIO (getNetworkMsg handle chan)
    mvGame <- forkGameThread handle chan isServer
    takeMVar mvGame -- Blocks until the game state thread returns
    killThread tid1
    killThread tid2