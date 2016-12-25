module Main where

import Computing
import Types
import Const
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

-- Создание одной частицы
initialPar :: Point -> Float -> Particle
initialPar pos t = (Particle pos (0,0) 0 0 (makeColor 0 (1 * (t - 1)) (1 * t) 0.7) 0 t) 

-- Создание всех частиц
initialPars :: [Particle]
initialPars = [(initialPar (x , y) 1) | x <- posxCalc , y <- posyCalc]


-- Заполнение координат частиц
posxCalc :: [Float]
posxCalc = [(startX - pRadius * 2000 * numX / 2), (startX - pRadius * 2000 * numX / 2 + pRadius * 2000) .. (startX + (pRadius * 2000 * numX / 2 - pRadius * 2000))]


posyCalc :: [Float]
posyCalc = [(startY - pRadius * 2000 * numY / 2), (startY - pRadius * 2000 * numY / 2 + pRadius * 2000) .. (startY + (pRadius * 2000 * numY / 2 - pRadius * 2000))]

--Создание границ
initialBorders :: [Border]
initialBorders = [(Border [(-boxX, -boxY) , (-boxX, boxY)] (makeColor 0 0 0 1) (1, 0)) , (Border [(-boxX, boxY) , (boxX , boxY)] (makeColor 0 0 0 1) (0, -1))
                , (Border [(boxX , boxY) , (boxX, -boxY)] (makeColor 0 0 0 1) (-1, 0)) , (Border [(boxX , -boxY) , (-boxX , -boxY)] (makeColor 0 0 0 1) (0, 1))]

-- Создание картинки
initialRoom :: Pic
initialRoom = (Pic initialPars 0 initialBorders 1)

-- Отрисовка одной частицы
drawPar :: Particle ->  Picture
drawPar (Particle (x , y) _ _ _ c _ _) = translate x y (scale parScale parScale (color c ( thickCircle (pRadius * 1000 / 2) (pRadius * 1000))))

--  Отрисовка прямоугольников
drawPol :: Picture
drawPol = (color (makeColor 0.5 0.8 0.8 1) (Polygon [( winSizeX / 2, winSizeY / 2) , ( -winSizeX / 2, winSizeY / 2) ,
                                                    ( -winSizeX / 2, boxY)]))

-- Отрисовка границ
drawBorder :: Border -> Picture
drawBorder (Border path c _) = (color c (line path))

-- Отрисовка картнки
drawPic :: Pic ->Picture
drawPic (Pic ps a bs s) = pictures ((map drawPar ps) ++ (drawPol : [])++ (map drawBorder bs))

handlePic :: Event -> Pic -> Pic
handlePic (EventKey (MouseButton LeftButton) Down _ (x , y)) = addPart (x , y)
handlePic (EventKey (SpecialKey KeyLeft) _ _ _)  = leftAcc
handlePic (EventKey (SpecialKey KeyRight) _ _ _)  = rightAcc
handlePic (EventKey (SpecialKey KeyUp) _ _ _)  = upAcc
handlePic (EventKey (SpecialKey KeyDown) _ _ _)  = downAcc
handlePic (EventMotion _)  = id
handlePic (EventResize _)  = id
handlePic (EventKey (Char 'r') _ _ _)  = restart
handlePic (EventKey (Char 'e') _ _ _)  = empty
handlePic (EventKey (MouseButton _) _ _ _)  = id
handlePic (EventKey (Char 's') _ _ _) = stop


leftAcc:: Pic -> Pic
leftAcc (Pic ps a bs s) = accPic
  where newa = if a == 1 then 0 else 1
        accPic = (Pic ps newa bs s)

downAcc:: Pic -> Pic
downAcc (Pic ps a bs s) = accPic
  where newa = if a == 2 then 0 else 2
        accPic = (Pic ps newa bs s)

upAcc:: Pic -> Pic
upAcc (Pic ps a bs s) = accPic
  where newa = if a == 4 then 0 else 4
        accPic = (Pic ps newa bs s)

rightAcc:: Pic -> Pic
rightAcc (Pic ps a bs s) = accPic
  where newa = if a == 3 then 0 else 3
        accPic = (Pic ps newa bs s)

-- Добавление новой частицы по нажатию левой клавиши мыши
addPart :: Point -> Pic -> Pic
addPart pos (Pic ps a bs s) = (Pic newPs a bs s)
  where
  	newPs = newPar : ps
  	newPar = initialPar pos 0

-- Создание пустой картинки по клавише 'e'
empty :: Pic -> Pic
empty (Pic _ a bs s) = (Pic [] a bs s)

-- Restart симуляции
restart :: Pic -> Pic
restart _ = initialRoom

stop :: Pic -> Pic
stop (Pic ps a bs s) = stPic
  where news = if s > 0 then 0 else 1
        stPic = (Pic ps a bs news)

updatePic :: Float -> Pic -> Pic
updatePic time (Pic ps a bs 1) = (Pic (newPar ps a bs (time * 10)) a bs 1)
updatePic time (Pic ps a bs 0) = (Pic ps a bs 0)
                           


main :: IO ()
main = do
  play display bgColor fps initialRoom drawPic handlePic updatePic
  where
    windowSize   = ((truncate winSizeX ) , (truncate winSizeY))
    windowOffset = (250, 250)
    display = InWindow "Fluids" windowSize windowOffset
    bgColor = (makeColor 0.1 0.8 0.8 1)
    fps = 60
