module Const where
import Types
import Graphics.Gloss.Interface.Pure.Game



-- Ширина окна
winSizeX :: Float
winSizeX = 720

-- Высота окна
winSizeY :: Float
winSizeY = 480

-- Ширина границ
boxX :: Float
boxX = 250

-- Высота границ
boxY :: Float
boxY = 220

-- Масса частицы
pMass::Fractional a => a
pMass = 0.05

-- Радиус частицы.
pRadius :: Fractional a => a
pRadius = 4.0

-- Длина сглаживания.
hSm::Fractional a => a
hSm = 0.022

-- Коэффициент поверхностного натяжения.
surfTens::Fractional a => a
surfTens = 0.0008

-- Коэффициент вязкости.
viscCoef::Fractional a => a
viscCoef = 0.0004

-- Коэффициент силы давления.
presCoef::Fractional a => a
presCoef = - 0.0008

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



