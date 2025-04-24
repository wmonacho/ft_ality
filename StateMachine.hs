module StateMachine where

import SDL
import qualified SDL.Event as Event
import SDL.Input.Keyboard

data KeyMap = KeyMap {
    keyCode :: Char,
    keyName :: String
} deriving (Show)

data StateMachine = StateMachine {
    keys :: [KeyMap],       -- Liste des touches valides
    states :: [[[String]]],   -- États intermédiaires (non utilisé ici)
    final_states :: [String] -- États finaux (non utilisé ici)
} deriving (Show)

-- Convertit un Char en Keycode SDL
charToKeycode :: Char -> Keycode
charToKeycode 'a' = KeycodeA
charToKeycode 'b' = KeycodeB
charToKeycode 'c' = KeycodeC
charToKeycode 'd' = KeycodeD
charToKeycode 'e' = KeycodeE
charToKeycode 'f' = KeycodeF
charToKeycode 'g' = KeycodeG
charToKeycode 'h' = KeycodeH
charToKeycode 'i' = KeycodeI
charToKeycode 'j' = KeycodeJ
charToKeycode 'k' = KeycodeK
charToKeycode 'l' = KeycodeL
charToKeycode 'm' = KeycodeM
charToKeycode 'n' = KeycodeN
charToKeycode 'o' = KeycodeO
charToKeycode 'p' = KeycodeP
charToKeycode 'q' = KeycodeQ
charToKeycode 'r' = KeycodeR
charToKeycode 's' = KeycodeS
charToKeycode 't' = KeycodeT
charToKeycode 'u' = KeycodeU
charToKeycode 'v' = KeycodeV
charToKeycode 'w' = KeycodeW
charToKeycode 'x' = KeycodeX
charToKeycode 'y' = KeycodeY
charToKeycode 'z' = KeycodeZ
charToKeycode '0' = Keycode0
charToKeycode '1' = Keycode1
charToKeycode '2' = Keycode2
charToKeycode '3' = Keycode3
charToKeycode '4' = Keycode4
charToKeycode '5' = Keycode5
charToKeycode '6' = Keycode6
charToKeycode '7' = Keycode7
charToKeycode '8' = Keycode8
charToKeycode '9' = Keycode9
charToKeycode ' ' = KeycodeSpace
charToKeycode '\t' = KeycodeTab
charToKeycode '\n' = KeycodeReturn
charToKeycode '\r' = KeycodeReturn
charToKeycode ',' = KeycodeComma
charToKeycode '.' = KeycodePeriod
charToKeycode '/' = KeycodeSlash
charToKeycode ';' = KeycodeSemicolon
charToKeycode '\'' = KeycodeUnknown
charToKeycode '[' = KeycodeLeftBracket
charToKeycode ']' = KeycodeRightBracket
charToKeycode '\\' = KeycodeBackslash
charToKeycode '-' = KeycodeMinus
charToKeycode '=' = KeycodeEquals
charToKeycode '`' = KeycodeBackquote
charToKeycode '!' = Keycode1
charToKeycode '@' = Keycode2
charToKeycode '#' = Keycode3
charToKeycode '$' = Keycode4
charToKeycode '%' = Keycode5
charToKeycode '^' = Keycode6
charToKeycode '&' = Keycode7
charToKeycode '*' = Keycode8
charToKeycode '(' = Keycode9
charToKeycode ')' = Keycode0
charToKeycode '_' = KeycodeMinus
charToKeycode '+' = KeycodeEquals
charToKeycode '{' = KeycodeLeftBracket
charToKeycode '}' = KeycodeRightBracket
charToKeycode '|' = KeycodeBackslash
charToKeycode ':' = KeycodeSemicolon
charToKeycode '"' = KeycodeUnknown
charToKeycode '<' = KeycodeComma
charToKeycode '>' = KeycodePeriod
charToKeycode '?' = KeycodeSlash
charToKeycode '~' = KeycodeBackquote
charToKeycode _   = KeycodeUnknown