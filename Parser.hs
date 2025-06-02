module Parser where

import StateMachine
import System.IO
import Data.List
import Data.List.Split (splitOn)
import Data.Char (isSpace, isAlphaNum)

removeAllWhitespace :: String -> String
removeAllWhitespace = f . f
    where f = reverse . dropWhile isSpace

removeSpacesAroundSpecials :: String -> String
removeSpacesAroundSpecials = go Nothing
  where
    specials = "+,="
    isSpecial c = c `elem` specials
    go :: Maybe Char -> String -> String
    go _ [] = []
    go prev (c:cs)
      | isSpace c =
          case (prev, cs) of
            (Just p, n:_) | isSpecial p -> go prev cs
            (_, n:_) | isSpecial n -> go prev cs
            _ -> c : go (Just c) cs
      | otherwise = c : go (Just c) cs

parseKey :: String -> IO KeyMap
parseKey line = do
    let new_line = removeSpacesAroundSpecials line
    let splited = splitOn "=" new_line
    let cleaned = map removeAllWhitespace splited
    case cleaned of
        [key_name, [c]] | isAlphaNum c && all isAlphaNum key_name -> return (KeyMap c key_name)
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
    let new_line = removeSpacesAroundSpecials line
    let splited = splitOn "=" new_line
    let cleaned = map removeAllWhitespace splited
    case cleaned of
        [combo_name, rest] -> do
            let final_splited = map (splitOn "+") (splitOn "," rest)
            if null combo_name || not (all isAlphaNum combo_name) || any (any null) final_splited then error ("Invalid line format: " ++ line)
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
        line <- hGetLine h
        if null (removeAllWhitespace line)
            then processCombos h
            else do
                r <- parseCombos line
                rest <- processCombos h
                return (r : rest)

generateFinals :: [Combo] -> [String]
generateFinals combos = map fst combos

generateMoves :: [Combo] -> [[[String]]]
generateMoves combos = map snd combos

mergeComboLists :: [[[String]]] -> [[[String]]] -> IO [[[String]]]
mergeComboLists [] [] = return []
mergeComboLists list_a list_b = return (list_a ++ list_b)

hasDuplicateNames :: [String] -> IO ()
hasDuplicateNames xs = 
    if length xs /= length (nub xs)
        then error ("Duplicated Key name")
        else return ()

hasDuplicateKeys :: String -> IO ()
hasDuplicateKeys xs = 
    if length xs /= length (nub xs)
        then error ("Duplicated Key code " ++ xs)
        else return ()

validateNestedMoves :: [String] -> [[[String]]] -> IO ()
validateNestedMoves valid_names nested = do
    let used_names = concatMap concat nested
    let invalid_names = filter (`notElem` valid_names) used_names
    if null invalid_names
        then return ()
        else error ("Invalid names found: " ++ show (nub invalid_names))
