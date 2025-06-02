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

-- Convertit un Char en Keycode SDL (alphanum uniquement)
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
charToKeycode _   = KeycodeUnknown

-- Convertit un Keycode SDL en Char (alphanum uniquement)
keycodeToChar :: Keycode -> Char
keycodeToChar KeycodeA = 'a'
keycodeToChar KeycodeB = 'b'
keycodeToChar KeycodeC = 'c'
keycodeToChar KeycodeD = 'd'
keycodeToChar KeycodeE = 'e'
keycodeToChar KeycodeF = 'f'
keycodeToChar KeycodeG = 'g'
keycodeToChar KeycodeH = 'h'
keycodeToChar KeycodeI = 'i'
keycodeToChar KeycodeJ = 'j'
keycodeToChar KeycodeK = 'k'
keycodeToChar KeycodeL = 'l'
keycodeToChar KeycodeM = 'm'
keycodeToChar KeycodeN = 'n'
keycodeToChar KeycodeO = 'o'
keycodeToChar KeycodeP = 'p'
keycodeToChar KeycodeQ = 'q'
keycodeToChar KeycodeR = 'r'
keycodeToChar KeycodeS = 's'
keycodeToChar KeycodeT = 't'
keycodeToChar KeycodeU = 'u'
keycodeToChar KeycodeV = 'v'
keycodeToChar KeycodeW = 'w'
keycodeToChar KeycodeX = 'x'
keycodeToChar KeycodeY = 'y'
keycodeToChar KeycodeZ = 'z'
keycodeToChar Keycode0 = '0'
keycodeToChar Keycode1 = '1'
keycodeToChar Keycode2 = '2'
keycodeToChar Keycode3 = '3'
keycodeToChar Keycode4 = '4'
keycodeToChar Keycode5 = '5'
keycodeToChar Keycode6 = '6'
keycodeToChar Keycode7 = '7'
keycodeToChar Keycode8 = '8'
keycodeToChar Keycode9 = '9'
keycodeToChar _ = '?'
