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
import Data.List (nub)

main :: IO ()
main = do
    -- Parsing of the file
    h <- openFile "Keys/simple.gmr" ReadMode 
    processed_key <- processKeys h
    hasDuplicateNames (map keyName processed_key)
    hasDuplicateKeys (map keyCode processed_key)
    processed_combo <- processCombos h
    let finals = generateFinals processed_combo
    let moves = generateMoves processed_combo
    validateNestedMoves ((map keyName processed_key) ++ finals) moves
    let state_machine = (StateMachine processed_key moves finals)
    let combos_init = states state_machine
    hClose h

    initializeAll

    -- Création de la fenêtre
    window <- createWindow "Key Input Detector" defaultWindow

    -- Lancement de la boucle principale
    appLoop state_machine [] [] combos_init False

    -- Fermeture de la fenêtre
    destroyWindow window
    quit
    quit