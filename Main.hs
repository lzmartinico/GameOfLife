import Grid
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.Matrix

main = do
	let d = InWindow "Game of Life" (300, 300) (10, 10)
	simulate d white 8 test render tick'

render :: Grid -> Picture
render = scale 5 5 . pictures . map f . findSquares
	where
	    f (a,b) = polygon [(a,b), (a+1, b), (a+1, b+1), (a, b+1)]

findSquares :: Grid -> [(Float, Float)]
findSquares g = [(fromIntegral x, fromIntegral y)|x <-[1..(ncols g)], y <-[1..(nrows g)], g ! (x,y) == 1]

tick' :: ViewPort -> Float -> Grid -> Grid
tick' _ _ = tick
