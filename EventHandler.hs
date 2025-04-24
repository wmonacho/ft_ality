module EventHandler where

import SDL
import qualified SDL.Event as Event
import SDL.Input.Keyboard
import StateMachine
import Utils
import Data.List (nub, (\\))

-- Fonction pour détecter les inputs valides
handleEvents :: StateMachine -> [Keycode] -> IO ([Keycode], Bool)
handleEvents stateMachine pressedBuffer = do
    events <- pollEvents
    let quitEvent = any isQuitEvent events
    let validKeys = map (charToKeycode . keyCode) (keys stateMachine) -- Convertir les chars en Keycode

    -- Récupérer les touches pressées et relâchées
    let pressedKeys = [keysymKeycode (keyboardEventKeysym e) | Event.KeyboardEvent e <- map eventPayload events, keyboardEventKeyMotion e == Pressed]
    let releasedKeys = [keysymKeycode (keyboardEventKeysym e) | Event.KeyboardEvent e <- map eventPayload events, keyboardEventKeyMotion e == Released]

    -- Filtrer les touches pressées pour ne garder que les touches valides
    let filteredPressedKeys = filter (`elem` validKeys) pressedKeys

    -- Mettre à jour le buffer des touches pressées
    let updatedBuffer = nub $ (pressedBuffer ++ filteredPressedKeys) \\ releasedKeys

    -- Détecter les touches valides relâchées
    let detectedReleasedKeys = filter (`elem` validKeys) releasedKeys

    -- Appeler une fonction pour obtenir les prochaines touches possibles pour un combo
    if not (null detectedReleasedKeys)
        then do
            putStrLn $ "Released keys: " ++ show detectedReleasedKeys
            putStrLn $ "Buffer before clearing: " ++ show pressedBuffer
            let nextPossibleCombos = getNextPossibleKeys stateMachine detectedReleasedKeys
            putStrLn $ "Next possible combos: " ++ show nextPossibleCombos
        else return ()

    return (updatedBuffer, quitEvent)

-- Vérifie si un événement est une demande de quitter
isQuitEvent :: Event -> Bool
isQuitEvent event =
    case eventPayload event of
        QuitEvent -> True
        KeyboardEvent keyboardEvent ->
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeEscape
        _ -> False

-- Boucle principale de l'application
appLoop :: StateMachine -> [Keycode] -> IO ()
appLoop stateMachine pressedBuffer = do
    (updatedBuffer, quit) <- handleEvents stateMachine pressedBuffer
    if quit
        then putStrLn "Quit event detected. Exiting..."
        else appLoop stateMachine updatedBuffer