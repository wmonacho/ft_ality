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

handleEvents :: StateMachine -> [Keycode] -> [[String]] -> [[[String]]] -> Bool -> IO ([Keycode], Bool, [[String]], [[[String]]], Bool)
handleEvents stateMachine pressedBuffer stepBuffer combos debug = do
    events <- pollEvents
    let quitEvent = any isQuitEvent events
    let validKeys = map (charToKeycode . keyCode) (keys stateMachine)

    let pressedKeys = [keysymKeycode (keyboardEventKeysym e) | Event.KeyboardEvent e <- map eventPayload events, keyboardEventKeyMotion e == Pressed]
    let releasedKeys = [keysymKeycode (keyboardEventKeysym e) | Event.KeyboardEvent e <- map eventPayload events, keyboardEventKeyMotion e == Released]

    let filteredPressedKeys = filter (`elem` validKeys) pressedKeys
    let updatedBuffer = nub $ (pressedBuffer ++ filteredPressedKeys) \\ releasedKeys
    let detectedReleasedKeys = filter (`elem` validKeys) releasedKeys

    let combos_init = states stateMachine
    let releasedKeyNames = getKeyName detectedReleasedKeys stateMachine
    let maxComboLen = maximum (map (\c -> length c - 1) combos_init)
    let trimToMax xs = drop (length xs - maxComboLen) xs

    let mKeyPressed = KeycodeM `elem` releasedKeys
    let newDebug = if mKeyPressed then not debug else debug

    let tryBuffer = if not (null releasedKeyNames) then trimToMax (stepBuffer ++ [releasedKeyNames]) else stepBuffer

    -- Vérifier si le buffer courant est préfixe d'un combo 
    let isValid buf = any (\combo -> buf `isStepPrefixOf` (init combo)) combos_init
    let appendAndTrim xs x = trimToMax (xs ++ [x])
    let newStepBuffer
            | null releasedKeyNames = stepBuffer
            | isValid tryBuffer     = tryBuffer
            | otherwise             = appendAndTrim stepBuffer releasedKeyNames

    -- Affichage
    let nextCombos = combos

    delay 25

    if not (null releasedKeyNames)
        then do
            let nextCombos = nextCombosDisplay newStepBuffer combos

            let matchedCombos = filter
                    (\combo ->
                        length combo == 1 &&
                        last (last combo) `elem` final_states stateMachine
                    )
                    nextCombos
            if debug
                then do
                    putStrLn $ "Released key names: " ++ show releasedKeyNames
                    putStrLn $ "Combos: " ++ show combos
                    putStrLn $ "New Step Buffer: " ++ show newStepBuffer
                    putStrLn "Prochaines transitions possibles :"
                    mapM_ print nextCombos
                    putStrLn $ "Matched Combos: " ++ show matchedCombos
                else return ()
            if not (null matchedCombos)
                then do
                    let finals = map (last . last) matchedCombos
                    mapM_ (\final ->
                        if final `elem` final_states stateMachine
                            then putStrLn $ rainbow (map toUpper final)
                            else return ()
                        ) finals
                else return ()
        else return ()
    return (updatedBuffer, quitEvent, newStepBuffer, nextCombos, newDebug)

isQuitEvent :: Event -> Bool
isQuitEvent event =
    case eventPayload event of
        QuitEvent -> True
        KeyboardEvent keyboardEvent ->
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeEscape
        _ -> False

appLoop :: StateMachine -> [Keycode] -> [[String]] -> [[[String]]] -> Bool -> IO ()
appLoop stateMachine pressedBuffer stepBuffer combos_init debug  = do
    (updatedBuffer, quit, newStepBuffer, combos, debug) <- handleEvents stateMachine pressedBuffer stepBuffer combos_init debug 
    if quit
        then putStrLn "Quit event detected. Exiting..."
        else appLoop stateMachine updatedBuffer newStepBuffer combos debug
