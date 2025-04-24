module Utils where

import Data.List (nub)
import SDL
import qualified SDL.Event as Event
import SDL.Input.Keyboard
import StateMachine

-- Fonction pour obtenir les prochaines touches possibles pour un combo
getNextPossibleKeys :: StateMachine -> [Keycode] -> [[[String]]]
getNextPossibleKeys (StateMachine _ states _) releasedKeys =
    case releasedKeys of
        [key] -> getCombosForKey key states
        _     -> [] -- Si plusieurs touches sont relâchées ou aucune, aucune étape suivante

-- Fonction auxiliaire pour récupérer les combos possibles pour une touche donnée
getCombosForKey :: Keycode -> [[[String]]] -> [[[String]]]
getCombosForKey key states = []