module Main where

import SinglePlayer
import MultiPlayer

main :: IO ()
main = do
    putStrLn $ "~~~~~~~~~~~~~~~\nWelcome to Capture The Mine!.\n~~~~~~~~~~~~~~~\n" ++
               "Type 'offline' to create an offline game or 'host' to host an online match!"
    str <- getLine
    case str of
        "offline" -> SinglePlayer.createGame
        "host" -> MultiPlayer.hostGame
        "exit" -> return ()
        _      -> putStrLn "Unknown command\n" >> main



