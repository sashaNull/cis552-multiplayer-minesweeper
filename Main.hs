module Main where

import Control.Concurrent
import Control.Exception (IOException, catch)
import Control.Monad
import Data.Maybe
import Multiplayer
import Network.BSD (getHostByName, hostAddress)
import Network.Socket
import Single
import System.IO

main :: IO ()
main = do
  putStrLn $
    "~~~~~~~~~~~~~~~\nWelcome to Capture The Mine.\n~~~~~~~~~~~~~~~\n"
      ++ "Type 'local' to create a local game, 'join' to join an existing game or 'host' to create a new game."
  str <- getLine
  case str of
    "local" -> Single.createGame
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
  conn <- accept sock -- accept a connection and handle it
  runConn conn -- run our server's logic
  handleConnections sock -- repeat

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering

  -- Redirect stdout to the handler
  hPutStrLn hdl "Hello, world!"

  -- Send the output to the handler
  hClose hdl

--     -- Set up handles for communication
--     hdl <- socketToHandle clientSock ReadWriteMode

--     -- Example: Send a welcome message to the client
--     hPutStrLn hdl "Welcome to the game! You are now connected."

--     forever $ do
--         msg <- hGetLine hdl
--         putStrLn $ "Received message from client: " ++ msg

--         -- Example: Send a response back to the client
--         hPutStrLn hdl "Message received successfully!"

--     -- Close the handles and the socket when the client disconnects
--     hClose hdl
--     close clientSock

-- -- Function to join an existing game by connecting to a host
-- joinGame :: IO ()
-- joinGame = withSocketsDo $ do
--     putStrLn "Enter the host's IP address:"
--     hostAddressStr <- getLine

--     -- Resolve the host's IP address
--     addrInfos <- getAddrInfo Nothing (Just hostAddressStr) (Just "4242")

--     -- Connect to the first available address
--     let serverAddr = addrAddress (head addrInfos)

--     -- Create a socket
--     sock <- socket (addrFamily (head addrInfos)) Stream defaultProtocol

--     -- Connect to the server
--     connect sock serverAddr

--     putStrLn "Connected to the game! You are now a player."

--     -- TODO: Implement game logic or further communication with the host

--     -- Start communication with the server
--     communicateWithServer sock

--     -- Close the socket when done
--     close sock

-- communicateWithServer :: Socket -> IO ()
-- communicateWithServer sock = do
--     hdl <- socketToHandle sock ReadWriteMode
--     -- Example: Send a message to the server
--     hPutStrLn hdl "Hello, server! I am a player."

--     -- Receive and print messages from the server in a loop
--     forever $ do
--         msg <- hGetLine hdl
--         putStrLn $ "Received message from server: " ++ msg
