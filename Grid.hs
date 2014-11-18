module Grid where

import Data.Matrix

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

--nthStep :: Grid -> Int -> Grid
--nthStep g n = iterate tick g !! n

--prettyNthStep :: Grid -> Int -> IO ()
--prettyNthStep = putStrLn . prettyMatrix . nthStep
