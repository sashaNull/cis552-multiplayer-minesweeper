module MultiPlayer where

import Control.Exception
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.State
import Network.Socket
import System.IO
import System.Random (Random (randomRs), RandomGen, getStdGen, newStdGen)

import BoardPrint
import Control.Concurrent
import Helpers
import Logic
import Parser

getPlayer :: Int -> Player
getPlayer 1 = Player1
getPlayer 2 = Player2
getPlayer _ = error "Invalid player number"

updateExploredMVar :: MVar Explored -> Board -> Location -> IO ()
updateExploredMVar exploredVar currentBoard loc = do
  currentExplored <- takeMVar exploredVar
  let newExplored = explore currentBoard loc currentExplored
  putMVar exploredVar newExplored

updateStateMVar :: MVar GameState -> State GameState () -> IO ()
updateStateMVar stateVar updateFunction = do
  currentState <- takeMVar stateVar
  let newState = execState (do updateFunction) currentState
  putMVar stateVar newState

printMatchMessage :: Int -> Chan Msg -> Handle -> MVar Explored -> MVar Board -> MVar GameState -> IO ()
printMatchMessage msgNum chan hdl exploredVar boardVar stateVar = do
    let broadcastToAll msg = do
          writeChan chan (msgNum, msg)
          hPutStrLn hdl msg
    currentExplored <- readMVar exploredVar
    currentBoard <- readMVar boardVar
    currentState <- readMVar stateVar
    broadcastToAll ("\nIt is now " ++ show (player currentState) ++ "'s turn.")
    broadcastToAll ("Player 1 Score: " ++ show (score1 currentState))
    broadcastToAll ("Player 2 Score: " ++ show (score2 currentState))
    broadcastToAll ("Nummber of Remaining Mines: " ++ show (countVisibleMine currentBoard - countVisibleMine currentExplored))
    broadcastToAll (showMatrixWith tile currentExplored)

printMatchEndMessage :: Int -> Chan Msg -> Handle -> MVar Explored -> MVar Board -> MVar GameState -> IO ()
printMatchEndMessage msgNum chan hdl exploredVar boardVar stateVar = do
    let broadcastToAll msg = do
          writeChan chan (msgNum, msg)
          hPutStrLn hdl msg
    currentExplored <- readMVar exploredVar
    currentBoard <- readMVar boardVar
    currentState <- readMVar stateVar
    broadcastToAll "\nTHE END!"
    broadcastToAll (show (player currentState) ++ " won the match! ")
    broadcastToAll ("Player 1 Score: " ++ show (score1 currentState))
    broadcastToAll ("Player 2 Score: " ++ show (score2 currentState))
    broadcastToAll ("Nummber of Remaining Mines: " ++ show (countVisibleMine currentBoard - countVisibleMine currentExplored))
    broadcastToAll (showMatrixWith tile currentExplored)


printMatchDrawMessage :: Int -> Chan Msg -> Handle -> MVar Explored -> MVar Board -> MVar GameState -> IO ()
printMatchDrawMessage msgNum chan hdl exploredVar boardVar stateVar = do
    let broadcastToAll msg = do
          writeChan chan (msgNum, msg)
          hPutStrLn hdl msg
    currentExplored <- readMVar exploredVar
    currentBoard <- readMVar boardVar
    currentState <- readMVar stateVar
    broadcastToAll "\nIt's a Draw! Play again!"
    broadcastToAll ("Player 1 Score: " ++ show (score1 currentState))
    broadcastToAll ("Player 2 Score: " ++ show (score2 currentState))
    broadcastToAll ("Nummber of Remaining Mines: " ++ show (countVisibleMine currentBoard - countVisibleMine currentExplored))
    broadcastToAll (showMatrixWith tile currentExplored)

hostGame :: IO ()
hostGame = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 31337 0)
  listen sock 2
  addr <- getSocketName sock
  putStrLn $ "Server listening on " ++ show addr
  putStrLn "\nFind the interface-specific IP address and use netcat or telnet to connect to this server!\ni.e. 'nc localhost 31337'"

  playerCountVar <- newMVar 0
  gameStatusVar <- newMVar 0
  conditionVar <- newEmptyMVar

  g <- getStdGen
  exploredVar <- newMVar $ Helpers.matrixMaker width height Unexplored -- nothing is explored
  boardVar <- newMVar $ genGame width height (width * height `div` 10) g
  stateVar <- newMVar initialState

  chan <- newChan 
  _ <- forkIO $ fix $ \loop -> do
    (_, _) <- readChan chan
    loop
  mainLoop sock chan playerCountVar gameStatusVar conditionVar exploredVar boardVar stateVar 0

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> MVar Int -> MVar Int -> MVar () -> MVar Explored -> MVar Board -> MVar GameState -> Int -> IO ()
mainLoop sock chan playerCountVar gameStatusVar conditionVar exploredVar boardVar stateVar msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan playerCountVar gameStatusVar conditionVar exploredVar boardVar stateVar msgNum)
  mainLoop sock chan playerCountVar gameStatusVar conditionVar exploredVar boardVar stateVar $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> MVar Int -> MVar Int -> MVar () -> MVar Explored -> MVar Board -> MVar GameState -> Int -> IO ()
runConn (sock, _) chan playerCountVar gameStatusVar conditionVar exploredVar boardVar stateVar msgNum = do
  hdl <- socketToHandle sock ReadWriteMode
  let broadcastToAll msg = do
        writeChan chan (msgNum, msg)
        hPutStrLn hdl msg

  hSetBuffering hdl NoBuffering

  currentCount <- readMVar playerCountVar
  when (currentCount == 2) ( do
    hPutStrLn hdl "The room is full!\n"
    hClose hdl -- close the handle 
   )
 

  hPutStrLn hdl "~~~~~~~~~~~~~~~\nWelcome to Capture The Mine.\n~~~~~~~~~~~~~~~\n"
  modifyMVar_
    playerCountVar
    ( \count -> do
        let newCount = count + 1 
        return newCount
    )

  currentCount <- readMVar playerCountVar
  let userID = currentCount
  hPutStrLn hdl ("You're Player " ++ show userID)

  when (currentCount == 2)
    ( do
        newG <- newStdGen
        modifyMVar_ boardVar (\_ -> return $ genGame width height (width * height `div` 10) newG)
        modifyMVar_ gameStatusVar (\_ -> return 0)
        modifyMVar_ exploredVar (\_ -> return $ Helpers.matrixMaker width height Unexplored)
        modifyMVar_ stateVar (\_ -> return initialState)
        printMatchMessage msgNum chan hdl exploredVar boardVar stateVar  
        putMVar conditionVar () 
    )

  commLine <- dupChan chan

  -- fork off a thread for reading from the duplicated channel
  reader <- forkIO $ fix $ \loop -> do
    (nextNum, line) <- readChan commLine
    when (msgNum /= nextNum) $ hPutStrLn hdl line
    loop


  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    currentCount <- readMVar playerCountVar
    currentState <- readMVar stateVar
    if currentCount == 1 || (getPlayer userID /= player currentState) then do
      if currentCount == 1 then do
        hPutStrLn hdl "\n(Waiting for the other player to join...)"
        takeMVar conditionVar 
        >> loop
      else
        when (getPlayer userID /= player currentState) (do 
          hPutStrLn hdl "\n(Waiting for the other player to make the move...)"
          takeMVar conditionVar 
          currentGameStatus <- readMVar gameStatusVar
          when (currentGameStatus == 0) (do loop))
    else do
      line <- hGetLine hdl
      let temp = doParse locationParser line
        in case temp of
            Nothing -> do hPutStrLn hdl "\n(Invalid input!)" >> loop
            Just loc -> do
              currentExplored <- readMVar exploredVar
              currentBoard <- readMVar boardVar
              let newExplored = explore currentBoard (fst loc) currentExplored
                in if newExplored == currentExplored
                  then do hPutStrLn hdl "\n(Location already explored!)" >> loop
                  else do
                    let oldMineCount = countVisibleMine currentExplored
                    updateExploredMVar exploredVar currentBoard (fst loc)
                    currentExplored <- readMVar exploredVar
                    let newMineCount = countVisibleMine currentExplored
                      in if newMineCount > oldMineCount
                          then 
                            if player currentState == Player1 
                              then do
                                  updateStateMVar stateVar updateScore1 
                                  currentState <- readMVar stateVar
                                  if winingCondition currentExplored currentState currentBoard
                                    then do
                                      printMatchEndMessage msgNum chan hdl exploredVar boardVar stateVar
                                  else
                                    if countVisibleMine currentExplored == countVisibleMine currentBoard
                                      then do
                                        printMatchDrawMessage msgNum chan hdl exploredVar boardVar stateVar
                                    else do
                                      printMatchMessage msgNum chan hdl exploredVar boardVar stateVar >> loop
                            else do 
                              updateStateMVar stateVar updateScore2
                              currentState <- readMVar stateVar
                              if winingCondition currentExplored currentState currentBoard
                                then do
                                  printMatchEndMessage msgNum chan hdl exploredVar boardVar stateVar
                              else
                                if countVisibleMine currentExplored == countVisibleMine currentBoard
                                  then do
                                    printMatchDrawMessage msgNum chan hdl exploredVar boardVar stateVar
                                else do
                                    printMatchMessage msgNum chan hdl exploredVar boardVar stateVar >> loop
                          else do 
                            updateStateMVar stateVar updatePlayer
                            printMatchMessage msgNum chan hdl exploredVar boardVar stateVar  
                            threadDelay 500000
                            putMVar conditionVar () 
                            threadDelay 500000
                            >> loop

  killThread reader -- kill after the loop ends
  broadcastToAll ("<-- Player" ++ show userID ++ " left.") -- make a final broadcast
  modifyMVar_
    playerCountVar
    ( \count -> do
        let newCount = count - 1
        return newCount
    )
  modifyMVar_
    gameStatusVar
    ( \_ -> do
        return 1
    )
  threadDelay 1000000
  currentState <- readMVar stateVar
  when (getPlayer userID == player currentState) (do putMVar conditionVar ())
  hClose hdl -- close the handle
