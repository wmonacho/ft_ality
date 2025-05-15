{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL hiding (trace)
import qualified SDL.Event as Event
import SDL.Input.Keyboard
import StateMachine
import EventHandler
import Utils
import Parser
import System.IO

main :: IO ()
main = do
    -- Parsing of the file
    h <- openFile "Keys/simple.gmr" ReadMode 
    processed_key <- processKeys h
    processed_combo <- processCombos h
    let finals = generateFinals processed_combo
    let moves = generateMoves processed_combo
    let state_machine = (StateMachine processed_key moves finals)
    hClose h

    -- Exemple of get state functions
    let key = ["DOWN"]
    print key
    res <- getStatesByKeys (states state_machine) key False
    print res

    initializeAll

    -- Exemple de StateMachine
    let keyMappings = [KeyMap 'w' "Up", KeyMap 's' "Down", KeyMap 'a' "Left", KeyMap 'd' "Right"]
    let states = [
            [["Up", "Down"], ["Up", "Left"]],
            [["Down", "Right"]],
            [["Left", "Right"]]
            ]
    let stateMachine = StateMachine keyMappings states []

    -- Création de la fenêtre
    window <- createWindow "Key Input Detector" defaultWindow

    -- Lancement de la boucle principale
    appLoop stateMachine []

    -- Fermeture de la fenêtre
    destroyWindow window
    quit