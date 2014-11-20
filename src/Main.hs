import Grid
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort(ViewPort)
import Data.Matrix
import Text.Printf
import System.Environment

type Time  = Float
type Coors = (Float, Float)

main = do
	args <- getArgs
	let
		stepsPerSec = read (head args) :: Int
		patternName = last args
		fileName = printf "../Patterns/%s.rle" patternName
	pattern <- fromFile fileName
	let d = InWindow "Game of Life" (700, 700) (10, 10)
	simulate d black stepsPerSec (pattern, 0) render tick'

render :: (Grid, Time) -> Picture
render (g, t) = translate (-200) (-200) . scale 5 5 . pictures . map f . findSquares $ g
	where
		-- f :: Coors -> Picture
	    f (a,b) = chaoticColor a b $ polygon [(a,b), (a+1, b), (a+1, b+1), (a, b+1)]
	    -- stockColors :: [Color]
	    stockColors = [red, green, blue, yellow, cyan, magenta, violet, rose, azure, aquamarine, orange, chartreuse]
	    -- chaoticColor :: Float -> Float -> Picture -> Picture
	    chaoticColor i j = color (stockColors !! chaos)
	    	where chaos = (floor (t * i * j)) `mod` 12

findSquares :: Grid -> [Coors]
findSquares g = [(fromIntegral x, fromIntegral y)|x <-[1..(nrows g)], y <-[1..(ncols g)], g ! (x,y) == 1]

tick' :: ViewPort -> Float -> (Grid, Time) -> (Grid, Time)
tick' _ t (g, _)  = (tick g, t)
