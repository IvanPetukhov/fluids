module Main where

import Lib
import Types
import Const
import Computing
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

import qualified Data.Foldable as Func

initialPar :: Point -> Particle
initialPar pos = (Particle pos  (0,0) (0,0) 0 0 (makeColor 0 0 1 1)) 

initialPars :: [Particle]
initialPars = [(initialPar (x , y)) | x <- [-100,-80..(150)] , y <- [-100,-80..(150)] ]

initialBorders :: [Border]
initialBorders = [(Border [(-7*k , -7*k) , (-7*k , 7*k)] (makeColor 0 0 0 1)) , (Border [(-7 *k, 7*k) , (7*k , 7*k)] (makeColor 0 0 0 1))
                , (Border [(7*k , 7*k) , (7*k, -7*k)] (makeColor 0 0 0 1)) , (Border [(7*k , -7*k) , (-7*k , -7*k)] (makeColor 0 0 0 1))]
initialRoom :: Pic
initialRoom = (Pic initialPars initialBorders)

drawPar :: Particle ->  Picture
drawPar (Particle (x , y)  _ _ _ _ c) = translate x y (scale 50 50 (color c ( thickCircle (2 * 0.02) 0.25)))

drawBorder :: Border -> Picture
drawBorder (Border path c) = (color c (line path))

drawPic :: Pic ->Picture
drawPic (Pic ps bs) = pictures ((Func.foldMap drawPar (reverse ps)) : (Func.foldMap drawBorder (reverse bs)) : [])


changePosL1 :: Particle->Particle
changePosL1 (Particle (x , y) a b c d e) = (Particle ((x-10) , y) a b c d e)

changePosR1 :: Particle->Particle
changePosR1 (Particle (x , y) a b c d e) = (Particle ((x+10) , y) a b c d e)

handlePic :: Event -> Pic -> Pic
handlePic (EventKey (SpecialKey KeyLeft) Down _ _)  = id
handlePic (EventKey (SpecialKey KeyRight) Down _ _)  = id
handlePic (EventMotion _)  = id
handlePic (EventResize _)  = id
handlePic (EventKey (Char _) _ _ _)  = id
handlePic (EventKey (MouseButton _) _ _ _)  = id

updatePar :: Float -> Particle -> [Border]->Particle
updatePar time (Particle (x , y) a b c d e) bs = (Particle (newPos (x , y) time bs) a b c d e)


newPos :: Point -> Float -> [Border] -> Point
newPos (x,y) time bs = map (check (x,y) time) bs 

check :: Point -> Float -> Border -> Maybe Point
check (x,y) time (Border [p1 p2] _) = intersectLineLine (x, y) (x+time, y) p1 p2

get :: [Maybe]

updatePic :: Float -> Pic -> Pic
updatePic time (Pic ps bs) = (Pic (map (updatePar time bs) ps) bs)


main :: IO ()
main = do
  play display bgColor fps initialRoom drawPic handlePic updatePic
  where
    windowSize   = (820 , 820)
    windowOffset = (250 , 250)
    display = InWindow "SPH Fluid" windowSize windowOffset
    bgColor = white
    fps = 60
