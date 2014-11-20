module Grid where

import Data.List
import Data.Matrix
import Text.Regex.Posix
import System.IO

type Cell = Int  -- in fact only 0 and 1 are permitted
type Grid = Matrix Cell

tick :: Grid -> Grid
tick old = newState 1 1 old $ zero m n
	where
		m = nrows old
		n = ncols old
		newState i j old underConstruction
			| j > n = newState (i+1) 1 old underConstruction
			| i > m = underConstruction
			| otherwise = newState i (j+1) old $ update i j old underConstruction

update :: Int -> Int -> Grid -> Grid -> Grid
update i j old underConstruction
	| ijCell == 0 && countAlive == 3 = setElem 1 (i,j) underConstruction
	| ijCell == 1 && (countAlive `elem` [2,3]) = setElem 1 (i,j) underConstruction
	| otherwise = underConstruction
	where
		ijCell = old ! (i,j)
		neighboursCoors = [(i-1, j-1), (i, j-1), (i+1, j-1), (i-1, j),
						(i+1, j), (i-1, j+1), (i, j+1), (i+1, j+1)]
		neighboursValues = map (getElem' old) neighboursCoors
		countAlive = sum [ 1 | (Just 1) <- neighboursValues ]

getElem' :: Grid -> (Int, Int) -> Maybe Cell
getElem' g (i,j)
	| i > 0 && j > 0 && i <= m && j <= n = Just $ getElem i j g
	| otherwise = Nothing
	where
		m = nrows g
		n = ncols g

glider :: Grid
glider = fromList 3 3 [0, 1, 0, 0, 0, 1, 1, 1, 1]

test :: Grid
test = joinBlocks (glider, zero 3 20, zero 20 3, zero 20 20)

nthStep :: Grid -> Int -> Grid
nthStep g n = iterate tick g !! n

-- conversion from RLE

type RLE = String
type RLELines = [String]
type RLEData = String

-- main = do
-- 	rle <- readFile "oscillator-syntheses.rle"
-- 	mapM print . getRowsData $ raw rle

fromFile :: FilePath -> IO Grid
fromFile filepath = do
	rle <- readFile filepath
	return $ fromRLE rle

fromRLE :: RLE -> Grid
fromRLE rle =
	let
		metadata = head $ raw rle
		n = getN metadata
		rows = makeRows n . getRowsData $ raw rle
	in
		fromLists rows

raw :: RLE -> RLELines
raw = filter (not . isComment) . lines

-- getRowsData :: RLELines -> [String]
-- getRowsData = axe '$' . removeExclamation . foldr (++) "" . tail
-- 	where removeExclamation s = if "!" `isSuffixOf` s then init s else s

isComment :: String -> Bool
isComment str = str == "" || "#C " `isPrefixOf` str

getN :: String -> Int
getN str =
	let
		groups = str =~ "x[[:space:]]*=[[:space:]]*([0-9]+)" :: [[String]]
		n = if null groups
			then error "Invalid `x=' line."
			else read (last . head $ groups) :: Int
	in
		n

getRowsData :: RLELines -> RLEData
getRowsData = foldr (++) "" . tail

-- axe :: Char -> String -> [String]
-- axe c s
-- 	| c `elem` s = part : axe c rest'
-- 	| otherwise  = [s]
-- 	where
-- 		(part, rest) = break (== c) s
-- 		rest' = if null rest then rest else tail rest

makeRows :: Int -> RLEData -> [[Cell]]
makeRows n "" = []
makeRows n s = addDead n row : makeRows n rest
	where (row, rest) = makeRow s

makeRow :: RLEData -> ([Cell], String)
makeRow ""  = ([], "")
makeRow "!" = ([], "")
makeRow ('b' : s) = (0 : fst (makeRow s), snd (makeRow s))
makeRow ('o' : s) = (1 : fst (makeRow s), snd (makeRow s))
makeRow ('$' : s) = ([], s)
makeRow s =
	let
		pat = "([0123456789]+)([bo$])"
		(_, _, rest, groups) = s =~ pat :: (String, String, String, [String])
		n = read (head groups) :: Int
		c = head $ last groups  -- get one of bo$ as a character
		expanded = replicate n c ++ rest
	in
		makeRow expanded
 
addDead :: Int -> [Cell] -> [Cell]
addDead n row = let missing = n - length row in row ++ replicate missing 0

-- makeRow' :: String -> [Cell]
-- makeRow' s = if null s then [] else
-- 	let
-- 		pat = "([0123456789]*)([bo])"
-- 		(_, _, rest, groups) = s =~ pat :: (String, String, String, [String])
-- 		nStr = if null groups then error "It's here" else head groups
-- 		n = if null nStr then 1 else read nStr :: Int
-- 		cell = if last groups == "b" then 0 else 1
-- 	in
-- 		replicate n cell ++ makeRow' rest

sample = "x = 3\nbob$bbo$ooo"