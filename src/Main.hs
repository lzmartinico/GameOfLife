import Grid
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort(ViewPort)
import Data.Matrix
import Text.Printf
import System.Environment
import System.Directory

type Time  = Float  --can we get rid of this?
type Colors = (Float, Float)   --renamed this Colors instead of Coors - typo or intentional

--need to be able to alternate between pattern and fileList when recognising a command such as "Options"  -is this possible?
--if not, delete lines 20,22-26
main = do
	args <- getArgs
	let
		stepsPerSec = read (head args) :: Int
		patternName = last args
		fileName = printf "../patterns/%s.rle" patternName
                fileNameList = getDirectoryContents "../patterns/"
	pattern <- fromFile fileName
	fileList <- fileNameList
        let 
            --this is necessary to remove hidden files; might be improved by checking if file ends with .rle, not sure how to do it though
            patternList = (filter f fileList)
            f x = length x > 4
        --return patternList      this should be the alternate function intead of simulate
        let d = InWindow "Game of Life" (1000, 1000) (10, 10)
	simulate d black stepsPerSec pattern render tick'

render :: Grid -> Picture
render = translate (-200) (-200) . scale 5 5 . pictures . map f . findSquares
	where
		--f :: Colors -> Picture
	    f (a,b) = chaoticColor a b $ polygon [(a,b), (a+1, b), (a+1, b+1), (a, b+1)]
	    -- stockColors :: [Color]
	    stockColors = [red, green, blue, yellow, cyan, magenta, violet, rose, azure, aquamarine, orange, chartreuse]
	    -- chaoticColor :: Float -> Float -> Picture -> Picture
	    chaoticColor i j = color (stockColors !! chaos)
	    	where chaos = (floor (i * j)) `mod` 12

findSquares :: Grid -> [Colors]
findSquares g = [(fromIntegral x, fromIntegral y)|x <-[1..(nrows g)], y <-[1..(ncols g)], g ! (x,y) == 1]

tick' :: ViewPort -> Float -> Grid -> Grid
tick' _ _  = tick
