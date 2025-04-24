{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL hiding (trace)
import qualified SDL.Event as Event
import SDL.Input.Keyboard
import StateMachine
import EventHandler
import Utils

main :: IO ()
main = do
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