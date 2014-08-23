import Debug.Trace
import Graphics.Gloss.Data.Color (makeColor', Color)
import Graphics.Gloss.Data.Display (Display(InWindow, FullScreen))
import Graphics.Gloss.Interface.Pure.Animate (animate)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Raster.Field (animateField, rgb8)

{-# INLINE cycleColor #-}
cycleColor :: Float -> Int
cycleColor t = mod ( abs $ round $ t * 10 ) 255

{-# INLINE cx #-}
{-# INLINE cy #-}
cx :: Float -> Float -> Float
cy :: Float -> Float -> Float
cx a time = a + 0.5 * sin ( time / 7 )
cy b time = b + 0.5 * cos ( time / 3 )

{-# INLINE bullseye #-}
{-# INLINE diagonals #-}
bullseye  :: Float -> Float -> Float -> Float
diagonals :: Float -> Float -> Float -> Float
bullseye  time a b = (1 + sin ( sqrt ( 100 * ((x ** 2) + (y ** 2)) + 1 + time) )) * 5
  where x = cx a time ; y = cy b time
diagonals time a b = (1 + sin ( 10 * ( a * sin ( time / 2 ) + b * cos (time / 3)) + time ) ) * 5

{-# INLINE frame #-}
frame :: Float -> Point -> Color
frame time point = rgb8 r g b
  where
    r = cycleColor ((diagonals' x y) + (bullseye' x y))
    g = cycleColor ((diagonals' y x) + (bullseye' y x))
    b = cycleColor ((diagonals' x x) + (bullseye' y y))
    {-r = cycleColor $ (0 + tan ((v/200) * pi)) * 20-}
    {-g = cycleColor $ (3 + sin ((v/40) * pi)) * 20-}
    {-b = cycleColor $ (1 + cos ((v/40) * pi)) * 20-}
    v = (diagonals' x y) + (bullseye' x y)
    x = fst point
    y = snd point
    bullseye'  = bullseye  time
    diagonals' = diagonals time

main :: IO ()
main = do
  animateField
    (InWindow "foo" (wid, hei) (10, 10))
    (scale, scale)
    frame
  where
    wid = 800
    hei = 600
    scale = 4
