{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import SDL hiding (trace)
import qualified SDL.Event as Event
import SDL.Input.Keyboard
import StateMachine
import EventHandler
import Utils
import Parser
import System.IO
import Control.Exception
import Data.List (nub)

printKeyMap :: KeyMap -> IO ()
printKeyMap km = putStrLn $ keyName km ++ " : " ++ [keyCode km]

printKeyMaps :: [KeyMap] -> IO ()
printKeyMaps = mapM_ printKeyMap

runProgram :: FilePath -> IO ()
runProgram file = do
    h <- openFile file ReadMode
    processed_key <- processKeys h
    hasDuplicateNames (map keyName processed_key)
    hasDuplicateKeys (map keyCode processed_key)
    processed_combo <- processCombos h
    let finals = generateFinals processed_combo
    let moves = generateMoves processed_combo
    validateNestedMoves ((map keyName processed_key) ++ finals) moves
    let state_machine = StateMachine processed_key moves finals
    let combos_init = states state_machine
    hClose h
    initializeAll
    putStrLn "Key maps:"
    --printKeyMaps processed_key
    window <- createWindow "Key Input Detector" defaultWindow
    appLoop state_machine [] [] combos_init False
    destroyWindow window
    quit
    quit

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            result <- try (runProgram file) :: IO (Either SomeException ())
            case result of
                Left e  -> putStrLn $ "An error occurred: " ++ show e
                Right _ -> return ()
        _ -> putStrLn "Usage: ./ft_ality <input_file>"