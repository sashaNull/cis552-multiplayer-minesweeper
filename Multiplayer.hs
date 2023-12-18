module MultiPlayer where

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

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 0)
  listen sock 2
  playerCountVar <- newMVar 0
  conditionVar <- newEmptyMVar

  g <- getStdGen
  exploredVar <- newMVar $ Helpers.matrixMaker width height Unexplored -- nothing is explored
  boardVar <- newMVar $ genGame width height (width * height `div` 10) g
  stateVar <- newMVar initialState

  chan <- newChan 
  _ <- forkIO $ fix $ \loop -> do
    (_, _) <- readChan chan
    loop
  mainLoop sock chan playerCountVar conditionVar exploredVar boardVar stateVar 0

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> MVar Int -> MVar () -> MVar Explored -> MVar Board -> MVar GameState -> Int -> IO ()
mainLoop sock chan playerCountVar conditionVar exploredVar boardVar stateVar msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan playerCountVar conditionVar exploredVar boardVar stateVar msgNum)
  mainLoop sock chan playerCountVar conditionVar exploredVar boardVar stateVar $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> MVar Int -> MVar () -> MVar Explored -> MVar Board -> MVar GameState -> Int -> IO ()
runConn (sock, _) chan playerCountVar conditionVar exploredVar boardVar stateVar msgNum = do
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
        g <- getStdGen
        modifyMVar_ exploredVar (\_ -> return $ Helpers.matrixMaker width height Unexplored)
        modifyMVar_ boardVar (\_ -> return $ genGame width height (width * height `div` 10) g)
        modifyMVar_ stateVar (\_ -> return initialState)
        broadcastToAll "\nAll players connected; game starting...\n\n"
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
    currentBoard <- readMVar boardVar
    currentExplored <- readMVar exploredVar
    if winingCondition currentExplored currentState currentBoard
      then do
        currentState <- readMVar stateVar
        currentExplored <- readMVar exploredVar
        hPutStrLn hdl "\n\nTHE END!"
        hPutStrLn hdl (show (player currentState) ++ "won the match! ")
        hPutStrLn hdl ("Player 1 Score: " ++ show (score1 currentState))
        hPutStrLn hdl ("Player 2 Score: " ++ show (score2 currentState))
        hPutStrLn hdl ("Nummber of Remaining Mines: " ++ show (countVisibleMine currentBoard - countVisibleMine currentExplored))
        hPutStrLn hdl (showMatrixWith tile currentExplored)
    else
      if countVisibleMine currentExplored == countVisibleMine currentBoard
        then do
          currentState <- readMVar stateVar
          currentExplored <- readMVar exploredVar
          hPutStrLn hdl "\n\nIt's a Draw! Play again!"
          hPutStrLn hdl ("Player 1 Score: " ++ show (score1 currentState))
          hPutStrLn hdl ("Player 2 Score: " ++ show (score2 currentState))
          hPutStrLn hdl ("Nummber of Remaining Mines: " ++ show (countVisibleMine currentBoard - countVisibleMine currentExplored))
          hPutStrLn hdl (showMatrixWith tile currentExplored)
      else do
        currentState <- readMVar stateVar
        currentExplored <- readMVar exploredVar
        hPutStrLn hdl ("\n\nIt is now " ++ show (player currentState) ++ "'s turn.")
        hPutStrLn hdl ("Player 1 Score: " ++ show (score1 currentState))
        hPutStrLn hdl ("Player 2 Score: " ++ show (score2 currentState))
        hPutStrLn hdl ("Nummber of Remaining Mines: " ++ show (countVisibleMine currentBoard - countVisibleMine currentExplored))
        hPutStrLn hdl (showMatrixWith tile currentExplored)
        hPutStrLn hdl "Please input Coordinate to explore, horizontal followed by vertical axis: (for example: 0 0)"
        if currentCount == 1 || (getPlayer userID /= player currentState) then do
          if getPlayer userID /= player currentState
          then do 
            hPutStrLn hdl "(Waiting for the other player to make the move...)"
            takeMVar conditionVar >> loop
          else do 
            hPutStrLn hdl "(Waiting for the other player to join...)"
            takeMVar conditionVar >> loop
        else do
          line <- hGetLine hdl
          let temp = doParse locationParser line
            in case temp of
                Nothing -> do hPutStrLn hdl "\n\n(Invalid input!)" >> loop
                Just loc -> do
                  let newExplored = explore currentBoard (fst loc) currentExplored
                    in if newExplored == currentExplored
                      then do hPutStrLn hdl "\n\n(Location already explored!)" >> loop
                      else do
                        let oldMineCount = countVisibleMine currentExplored
                        updateExploredMVar exploredVar currentBoard (fst loc)
                        currentExplored <- readMVar exploredVar
                        let newMineCount = countVisibleMine currentExplored
                          in if newMineCount > oldMineCount
                              then 
                                if player currentState == Player1 
                                  then do
                                      writeChan chan (msgNum, "\n\nIt is now " ++ show (player currentState) ++ "'s turn.")
                                      writeChan chan (msgNum, "Player 1 Score: " ++ show (score1 currentState))
                                      writeChan chan (msgNum, "Player 2 Score: " ++ show (score2 currentState))
                                      writeChan chan (msgNum, "Nummber of Remaining Mines: " ++ show (countVisibleMine currentBoard - countVisibleMine currentExplored))
                                      writeChan chan (msgNum, showMatrixWith tile currentExplored)
                                      writeChan chan (msgNum, "Please input Coordinate to explore, horizontal followed by vertical axis: (for example: 0 0)")
                                      updateStateMVar stateVar updateScore1 >> loop
                                else do 
                                  writeChan chan (msgNum, "\n\nIt is now " ++ show (player currentState) ++ "'s turn.")
                                  writeChan chan (msgNum, "Player 1 Score: " ++ show (score1 currentState))
                                  writeChan chan (msgNum, "Player 2 Score: " ++ show (score2 currentState))
                                  writeChan chan (msgNum, "Nummber of Remaining Mines: " ++ show (countVisibleMine currentBoard - countVisibleMine currentExplored))
                                  writeChan chan (msgNum, showMatrixWith tile currentExplored)
                                  writeChan chan (msgNum, "Please input Coordinate to explore, horizontal followed by vertical axis: (for example: 0 0)")
                                  updateStateMVar stateVar updateScore2 >> loop
                              else do 
                                updateStateMVar stateVar updatePlayer
                                putMVar conditionVar () >> loop

  killThread reader -- kill after the loop ends
  broadcastToAll ("<-- Player" ++ show userID ++ " left.") -- make a final broadcast
  modifyMVar_
    playerCountVar
    ( \count -> do
        let newCount = count - 1
        return newCount
    )
  hClose hdl -- close the handle
