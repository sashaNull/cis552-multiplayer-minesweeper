module MultiPlayer where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)

import System.Random (Random (randomRs), RandomGen, getStdGen)
import Logic
import Helpers

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 0)
  listen sock 2
  playerCountVar <- newMVar 0 
  chan <- newChan
  _ <- forkIO $ fix $ \loop -> do
    (_, _) <- readChan chan
    loop
  mainLoop sock chan playerCountVar 0

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> MVar Int -> Int -> IO ()
mainLoop sock chan playerCountVar msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan playerCountVar msgNum)
  mainLoop sock chan playerCountVar $! msgNum + 1


runConn :: (Socket, SockAddr) -> Chan Msg -> MVar Int-> Int -> IO ()
runConn (sock, _) chan playerCountVar msgNum = do
    hdl <- socketToHandle sock ReadWriteMode
    let broadcastToAll msg = do
          writeChan chan (msgNum, msg)
          hPutStrLn hdl msg

    hSetBuffering hdl NoBuffering

    hPutStrLn hdl "~~~~~~~~~~~~~~~\nWelcome to Capture The Mine.\n~~~~~~~~~~~~~~~\n" 
    hPutStrLn hdl "Please enter your name?"
    name <- hGetLine hdl
    broadcastToAll ("--> " ++ name ++ " entered the game.")

    modifyMVar_ playerCountVar (\count -> do
        let newCount = count + 1
        return newCount)
    
    currentCount <- readMVar playerCountVar

    when (currentCount == 2) (do
      g <- getStdGen
      let explored = Helpers.matrixMaker width height Unexplored -- nothing is explored
      let board = genGame width height (width * height `div` 10) g
      let boardString = showMatrixWith tile explored
      broadcastToAll "\nAll players connected; game starting\n."
      broadcastToAll boardString
      )


    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn hdl line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- hGetLine hdl
        currentCount <- readMVar playerCountVar
        if currentCount == 1 then do hPutStrLn hdl " (waiting for one more player...)" >> loop else do
          case line of
              -- If an exception is caught, send a message and break the loop
              "quit" -> hPutStrLn hdl "Bye!"
              -- else, continue looping.
              _      -> broadcastToAll (name ++ ": " ++ line) >> loop

    killThread reader                      -- kill after the loop ends
    broadcastToAll ("<-- " ++ name ++ " left.") -- make a final broadcast
    hClose hdl                             -- close the handle
