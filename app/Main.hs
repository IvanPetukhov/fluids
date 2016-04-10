module Main where

import Lib
import Computing
import Types
import Const
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

-- Создание одной частицы
initialPar :: Point -> Particle
initialPar pos = (Particle pos  (0,0) (0,0) 0 0 (makeColor 0 0 1 1)) 

-- Создание всех частиц
initialPars :: [Particle]
initialPars = [(initialPar (x , y)) | x <- [-100,-80..(150)] , y <- [-150,-130..(100)] ]

--Создание границ
initialBorders :: [Border]
initialBorders = [(Border [(-boxX, -boxY) , (-boxX, boxY)] (makeColor 0 0 0 1) (1, 0)) , (Border [(-boxX, boxY) , (boxX , boxY)] (makeColor 0 0 0 1) (0, -1))
                , (Border [(boxX , boxY) , (boxX, -boxY)] (makeColor 0 0 0 1) (-1, 0)) , (Border [(boxX , -boxY) , (-boxX , -boxY)] (makeColor 0 0 0 1) (0,1))]

-- Создание картинки
initialRoom :: Pic
initialRoom = (Pic initialPars initialBorders)

-- Отрисовка одной частицы
drawPar :: Particle ->  Picture
drawPar (Particle (x , y)  _ _ _ _ c) = translate x y (scale 2 2 (color c ( thickCircle pRadius  (pRadius * 2))))

-- Щтрисовка прямоугольников
drawPol1 :: Picture
drawPol1 = (color (makeColor 1 0.8 0.8 1) (Polygon [( -winSizeX / 2, -winSizeY / 2) , ( -winSizeX / 2, winSizeY / 2) ,
                                                    ( -boxX, winSizeY / 2) , ( -boxX, -winSizeY / 2)]))
drawPol2 :: Picture
drawPol2 = (color (makeColor 1 0.8 0.8 1) (Polygon [( -winSizeX / 2, -winSizeY / 2) , ( -winSizeX / 2, -boxY) ,
                                                    ( winSizeX / 2, -boxY) , ( winSizeX / 2, -winSizeY / 2)]))
drawPol3 :: Picture
drawPol3 = (color (makeColor 1 0.8 0.8 1) (Polygon [( boxX, -winSizeY / 2) , ( winSizeX / 2, -winSizeY / 2) ,
                                                    ( winSizeX / 2, winSizeY / 2) , ( boxX, winSizeY / 2)]))

drawPol4 :: Picture
drawPol4 = (color (makeColor 1 0.8 0.8 1) (Polygon [( winSizeX / 2, winSizeY / 2) , ( -winSizeX / 2, winSizeY / 2) ,
                                                    ( -winSizeX / 2, boxY) , ( winSizeX / 2, boxY)]))

-- Отрисовка границ
drawBorder :: Border -> Picture
drawBorder (Border path c _) = (color c (line path))

-- Отрисовка картнки
drawPic :: Pic ->Picture
drawPic (Pic ps bs) = pictures ((map drawPar ps) ++ (drawPol1 : drawPol2 : drawPol3 : drawPol4 : [])++ (map drawBorder bs))

handlePic :: Event -> Pic -> Pic
handlePic (EventKey (MouseButton LeftButton) Down _ (x , y)) = addPart (x , y )
handlePic (EventKey (SpecialKey KeyLeft) Down _ _)  = id
handlePic (EventKey (SpecialKey KeyRight) Down _ _)  = id
handlePic (EventMotion _)  = id
handlePic (EventResize _)  = id
handlePic (EventKey (Char 'r') _ _ _)  = restart
handlePic (EventKey (Char 'e') _ _ _)  = empty
handlePic (EventKey (MouseButton _) _ _ _)  = id

-- Добавление новой частицы по нажатию левой клавиши мыши
addPart :: Point -> Pic -> Pic
addPart pos (Pic ps bs) = (Pic newPs bs)
  where
  	newPs = newPar : ps
  	newPar = initialPar pos 

-- Создание пустой картинки по клавише 'e'
empty :: Pic -> Pic
empty (Pic _ bs) = (Pic [] bs)

-- Restart симуляции
restart :: Pic -> Pic
restart _ = initialRoom


updatePic :: Float -> Pic -> Pic
updatePic time (Pic ps bs) = (Pic (newPar ps bs (time * 5)) bs)
                           


main :: IO ()
main = do
  play display bgColor fps initialRoom drawPic handlePic updatePic
  where
    windowSize   = ((truncate winSizeX ) , (truncate winSizeY))
    windowOffset = (250 , 250)
    display = InWindow "Fluids" windowSize windowOffset
    bgColor = (makeColor 1.0 0.8 0.8 1)
    fps = 30