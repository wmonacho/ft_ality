module Utils where

import Data.List (nub, sort, tails)
import SDL
import qualified SDL.Event as Event
import SDL.Input.Keyboard
import StateMachine

-- recupere les noms des touches à partir de leurs Keycodes et de la liste des KeyMaps
getKeyName :: [Keycode] -> StateMachine -> [String]
getKeyName keycodes (StateMachine keyMaps _ _) =
    let chars = map keycodeToChar keycodes
    in map (\c -> case lookupKeyName c keyMaps of
                    Just name -> name
                    Nothing   -> "Unknown") chars

-- recherche le nom d'une touche à partir de son caractère
lookupKeyName :: Char -> [KeyMap] -> Maybe String
lookupKeyName char keyMaps =
    case filter (\km -> keyCode km == char) keyMaps of
        (km:_) -> Just (keyName km)
        []     -> Nothing

-- vérifie si une liste de chaînes de caractères est préfixe d'un combo
isStepPrefixOf :: [[String]] -> [[String]] -> Bool
isStepPrefixOf [] _ = True
isStepPrefixOf _ [] = False
isStepPrefixOf (x:xs) (y:ys) = sort x == sort y && isStepPrefixOf xs ys

-- applique des couleurs à une chaîne de caractères pour l'affichage dans le terminal caractère par caractère (avancement arc-en-ciel)
rainbow :: String -> String
rainbow str = concat $ zipWith (\c color -> "\x1b[" ++ show color ++ "m" ++ [c]) str (cycle [31,33,32,36,34,35,37]) ++ ["\x1b[0m"]

-- génère toutes les combinaisons de combos qui ont commencé à partir du buffer courant
startedCombos :: [[String]] -> [[[String]]] -> [[[String]]]
startedCombos buffer combos =
    nub [ drop (length suff) combo
        | combo <- combos
        , suff <- nonEmptySuffixes buffer
        , isStepPrefixOf suff combo
        ]
  where
    nonEmptySuffixes xs = filter (not . null) (tails xs)

-- retourne toutes les combinaisons de combos initiales
allCombos :: [[[String]]] -> [[[String]]]
allCombos = id

-- Affiche les prochaines combinaisons possibles ++ toutes les combinaisons de depart
nextCombosDisplay :: [[String]] -> [[[String]]] -> [[[String]]]
nextCombosDisplay buffer combos = nub (startedCombos buffer combos ++ allCombos combos)