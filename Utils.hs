module Utils where

import Data.List (nub, sort, tails)
import SDL
import qualified SDL.Event as Event
import SDL.Input.Keyboard
import StateMachine

-- Fonction pour récupérer le(s) keyName(s) correspondant(s) à un ou plusieurs Keycode(s)
getKeyName :: [Keycode] -> StateMachine -> [String]
getKeyName keycodes (StateMachine keyMaps _ _) =
    let chars = map keycodeToChar keycodes -- Convertir les Keycode en Char
    in map (\c -> case lookupKeyName c keyMaps of
                    Just name -> name
                    Nothing   -> "Unknown") chars

-- Fonction auxiliaire pour rechercher un keyName dans la liste KeyMap
lookupKeyName :: Char -> [KeyMap] -> Maybe String
lookupKeyName char keyMaps =
    case filter (\km -> keyCode km == char) keyMaps of
        (km:_) -> Just (keyName km) -- Retourne le premier keyName correspondant
        []     -> Nothing           -- Aucun keyName trouvé
    
-- Compare deux [[String]] en ignorant l'ordre des touches dans chaque étape
isStepPrefixOf :: [[String]] -> [[String]] -> Bool
isStepPrefixOf [] _ = True
isStepPrefixOf _ [] = False
isStepPrefixOf (x:xs) (y:ys) = sort x == sort y && isStepPrefixOf xs ys

rainbow :: String -> String
rainbow str = concat $ zipWith (\c color -> "\x1b[" ++ show color ++ "m" ++ [c]) str (cycle [31,33,32,36,34,35,37]) ++ ["\x1b[0m"]

-- Pour chaque suffixe non vide du buffer, si c'est un préfixe du combo, on ajoute la suite à faire
startedCombos :: [[String]] -> [[[String]]] -> [[[String]]]
startedCombos buffer combos =
    nub [ drop (length suff) combo
        | combo <- combos
        , suff <- nonEmptySuffixes buffer
        , suff `isStepPrefixOf` combo
        ]
  where
    nonEmptySuffixes xs = filter (not . null) (tails xs)

allCombos :: [[[String]]] -> [[[String]]]
allCombos = id

nextCombosDisplay :: [[String]] -> [[[String]]] -> [[[String]]]
nextCombosDisplay buffer combos = nub (startedCombos buffer combos ++ allCombos combos)