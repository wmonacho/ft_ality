module EventHandler where

import SDL
import SDL.Time
import qualified SDL.Event as Event
import SDL.Input.Keyboard
import StateMachine
import Utils
import Data.List (nub, (\\), isPrefixOf)
import Data.Char (toUpper)

type ComboProgress = [Int] -- Un index par combo

handleEvents :: StateMachine -> [[String]] -> [[[String]]] -> Bool -> IO (Bool, [[String]], [[[String]]], Bool)
handleEvents stateMachine stepBuffer combos debug = do
    events <- pollEvents
    let quitEvent = any isQuitEvent events
    let validKeys = map (charToKeycode . keyCode) (keys stateMachine)

    let pressedKeys = [keysymKeycode (keyboardEventKeysym e) | Event.KeyboardEvent e <- map eventPayload events, keyboardEventKeyMotion e == Pressed]
    let releasedKeys = [keysymKeycode (keyboardEventKeysym e) | Event.KeyboardEvent e <- map eventPayload events, keyboardEventKeyMotion e == Released]

    -- verifie que les touches pressées sont dans la liste des touches valides
    let filteredPressedKeys = filter (`elem` validKeys) pressedKeys
    let detectedReleasedKeys = filter (`elem` validKeys) releasedKeys

    let combos_init = states stateMachine
    -- recupere les noms des touches relachées
    let releasedKeyNames = getKeyName detectedReleasedKeys stateMachine
    let maxComboLen = maximum (map (\c -> length c - 1) combos_init)

    let mKeyPressed = elem KeycodeM releasedKeys
    let newDebug = if mKeyPressed then not debug else debug

    let trimToMax xs = drop (length xs - maxComboLen) xs
    -- Essaye d'ajouter la liste des touches relachées au buffer courant si non vide et la trim si besoin
    let newStepBuffer = if not (null releasedKeyNames) then trimToMax (stepBuffer ++ [releasedKeyNames]) else stepBuffer
    -- Affichage
    let nextCombos = combos

    delay 25

    if not (null releasedKeyNames)
        then do
            -- recuperation des toutes les possibilites de combos
            let nextCombos = nextCombosDisplay newStepBuffer combos

            -- filtrage des combos qui matchent (qui sont finis)
            let matchedCombos = filter
                    (\combo ->
                        length combo == 1 &&
                        last (last combo) `elem` final_states stateMachine
                    )
                    nextCombos
            if debug
                then do
                    putStrLn ("Released key names: " ++ show releasedKeyNames)
                    putStrLn ("Combos: " ++ show combos)
                    putStrLn ("New Step Buffer: " ++ show newStepBuffer)
                    putStrLn "Prochaines transitions possibles :"
                    mapM_ print nextCombos
                    putStrLn ("Matched Combos: " ++ show matchedCombos)
                else putStrLn ("Pressed: " ++ show releasedKeyNames)

            -- si un combo est matché, affiche le nom du combo en majuscule et en couleur
            if not (null matchedCombos)
                then do
                    let finals = map (last . last) matchedCombos
                    mapM_ (\final ->
                        if final `elem` final_states stateMachine
                            then putStrLn (rainbow (map toUpper final))
                            else return ()
                        ) finals
                else return ()
        else return ()
    return (quitEvent, newStepBuffer, nextCombos, newDebug)

isQuitEvent :: Event -> Bool
isQuitEvent event =
    case eventPayload event of
        QuitEvent -> True
        KeyboardEvent keyboardEvent ->
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeEscape
        _ -> False

appLoop :: StateMachine -> [[String]] -> [[[String]]] -> Bool -> IO ()
appLoop stateMachine stepBuffer combos_init debug  = do
    ( quit, newStepBuffer, combos, debug) <- handleEvents stateMachine stepBuffer combos_init debug 
    if quit
        then putStrLn "Quit event detected. Exiting..."
        else appLoop stateMachine newStepBuffer combos debug
