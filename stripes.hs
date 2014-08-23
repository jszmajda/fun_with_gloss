import Debug.Trace
import Graphics.Gloss.Data.Color (makeColor', Color)
import Graphics.Gloss.Data.Display (Display(InWindow, FullScreen))
import Graphics.Gloss.Interface.Pure.Animate (animate)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Raster.Field (animateField, rgb8)

{-# INLINE cycleColor #-}
cycleColor :: Float -> Int
cycleColor t = mod ( abs.round $ t * 10 ) 255

{-# INLINE bandColor #-}
bandColor :: Float -> Int
bandColor v = table !! (mod ranged $ length table)
  where
    ranged = (abs.round $ v * 1)
    table = [0, 0, 0, 0, 0, 60, 120, 255, 255, 255, 255, 255, 255]

{-# INLINE cx #-}
{-# INLINE cy #-}
cx :: Float -> Float -> Float
cy :: Float -> Float -> Float
cx a time = a + 0.5 * sin ( time / 7 )
cy b time = b + 0.5 * cos ( time / 3 )

{-# INLINE bullseye'' #-}
{-# INLINE bullseye #-}
{-# INLINE diagonals #-}
{-bullseye'' :: Float -> Float -> Float -> Float-}
bullseye'' time a b xmul ymul = (1 + sin ( sqrt ( 100 * (((abs x) ** xmul) + ((abs y) ** ymul)) + 1 + time) )) * 5
  where x = cx a time ; y = cy b time
bullseye  :: Float -> Float -> Float -> Float
diagonals :: Float -> Float -> Float -> Float
bullseye  time a b = bullseye'' time a b xmul ymul
  where
    xmul = 2
    ymul = 3
diagonals time a b = (1 + sin ( 10 * ( a * sin ( time / 2 ) + b * cos (time / 5)) ) ) * 4

{-# INLINE frame #-}
frame :: Float -> Point -> Color
frame time point = rgb8 r g b
  where
    -- r = bandColor ((diagonals' x y) + (bullseye' x y))
    -- g = bandColor ((diagonals' y x) + (bullseye' y x))
    -- b = bandColor ((diagonals' x x) + (bullseye' y y))
    {-r = cycleColor $ (0 + tan ((v/200) * pi)) * 20-}
    {-g = cycleColor $ (3 + sin ((v/40) * pi)) * 20-}
    {-b = cycleColor $ (1 + cos ((v/40) * pi)) * 20-}
    {-v = (diagonals' x y) + (bullseye' x y) -}
    -- v = bandColor ((diagonals' x y) + (bullseye' x y))
    v = bandColor $ (diagonals' x y) + (bullseye' x y)
    r = bandColor (bullseye' x y)
    g = v `div` (((round (time*10)) `mod` 6) + 1)
    b = v
    px = fst point
    py = snd point
    x = px * ct - py * st + st
    y = px * st + py * ct + ct
    theta = time / 10
    st = sin theta
    ct = cos theta
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
