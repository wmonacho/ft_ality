{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL hiding (trace)
import qualified SDL.Event as Event
import SDL.Input.Keyboard
import StateMachine
import EventHandler

main :: IO ()
main = do
    initializeAll

    -- Exemple de StateMachine
    let keyMappings = [KeyMap 'w' "Up", KeyMap 's' "Down"]
    let stateMachine = StateMachine keyMappings [] []

    window <- createWindow "Key Input Detector" defaultWindow
    appLoop stateMachine []
    destroyWindow window
    quit
    -- faire une exec  pour la strat d'ali,
    -- recap de ce quelle doit faire : 