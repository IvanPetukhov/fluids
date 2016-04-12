module Const where
import Types
import Graphics.Gloss.Interface.Pure.Game

--Начальная позиция по Х
startX :: Float
startX = -100

--Расстояние по Х между частицами
disX :: Float
disX = pRadius * 1000 * 2

--Начальная позиция по У
startY :: Float
startY = -150

--Расстояние по У между частицами
disY :: Float
disY = pRadius * 1000 * 2

--Количество частиц по Х
numX :: Float
numX = 8

--Количество частиц по У
numY :: Float
numY = 8

-- Ширина окна
winSizeX :: Float
winSizeX = 720

-- Высота окна
winSizeY :: Float
winSizeY = 620

-- Ширина границ
boxX :: Float
boxX = 200

-- Высота границ
boxY :: Float
boxY = 150

-- Масштаб отрисовки частицы
parScale :: Float
parScale = 1.5

-- Масса частицы
pMass::Fractional a => a
pMass = 0.05

-- Радиус частицы.
pRadius :: Fractional a => a
pRadius = 0.01

-- Длина сглаживания.
hSm::Fractional a => a
hSm = pRadius * 2 + 0.005

-- Коэффициент поверхностного натяжения.
surfTens::Fractional a => a
surfTens = 0.0008

-- Коэффициент вязкости.
viscCoef::Fractional a => a
viscCoef = 0.001

-- Коэффициент силы давления.
presCoef::Fractional a => a
presCoef = - 0.0001

--  Отскок от стены.
wallCoef::Fractional a => a
wallCoef = 30

--  Потеря энергии при отскоке.
wallEn::Fractional a => a
wallEn = - 2

-- Ускорение свободного падения
grav :: Point
grav = (0, -9.8)

-- Коэффициент жесткости среды
hardCoef::Fractional a => a
hardCoef = 1.0

-- Плотность среды
defDen::Fractional a => a
defDen = 998.0



