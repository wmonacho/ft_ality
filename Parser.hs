module Parser where

import StateMachine
import System.IO
import Data.List
import Data.List.Split (splitOn)
import Data.Char (isSpace, isAlphaNum)

removeAllWhitespace :: String -> String
removeAllWhitespace = filter (not . isSpace)

parseKey :: String -> IO KeyMap
parseKey line = do
    let splited = splitOn "=" line
    let cleaned = map removeAllWhitespace splited
    case cleaned of
        [key_name, [c]] | isAlphaNum c -> return (KeyMap c key_name)
        _ -> error ("Invalid line format: " ++ line)

splitCombo :: [String] -> String -> [String]
splitCombo [] final = [final]
splitCombo (x:"+":xs) final = x : splitCombo xs final
splitCombo (x:",":xs) final = x : splitCombo xs final
splitCombo [x] final = [x, final]
splitCombo xs _ = error ("Invalid format in keys: " ++ unwords xs)

addEllList :: [String] -> String -> [String]
addEllList xs v = [v] ++ xs

parseCombos :: String -> IO (String, [[String]])
parseCombos line = do
    let splited = splitOn "=" line
    let cleaned = map removeAllWhitespace splited
    case cleaned of
        [combo_name, rest] -> do
            let final_splited = map (splitOn "+") (splitOn "," rest)
            if null combo_name || any (any null) final_splited then error ("Invalid line format: " ++ line)
            else return (combo_name, final_splited ++ [[combo_name]])
        _ -> error ("Invalid line format: " ++ line)

processKeys :: Handle -> IO [KeyMap]
processKeys h = do
    xs <- hGetLine h
    if null xs
        then return []
    else do
        p <- parseKey xs
        rest <- processKeys h
        return (p : rest)

type Combo = (String, [[String]])

processCombos :: Handle -> IO [Combo]
processCombos h = do
    is_eof <- hIsEOF h
    if is_eof
        then return []
    else do
        xs <- hGetLine h
        r <- parseCombos xs
        rest <- processCombos h
        return (r : rest)

generateFinals :: [Combo] -> [String]
generateFinals combos = map fst combos

generateMoves :: [Combo] -> [[[String]]]
generateMoves combos = map snd combos

mergeComboLists :: [[[String]]] -> [[[String]]] -> IO [[[String]]]
mergeComboLists [] [] = return []
mergeComboLists list_a list_b = return (list_a ++ list_b)
