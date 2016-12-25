module Types where
import Graphics.Gloss.Interface.Pure.Game

data Particle = Particle 
                    {
                        position:: Point, -- Позиция
                        velocity:: Point, -- Скорость
                        pressure:: Float, -- Давление
                        density:: Float,  -- Плотность
                        col:: Color,      -- Цвет
                        a :: Float,       -- Наличие доп ускорения
                        t :: Float       -- тип жидкости
                    }

data Border = Border
                    {
                        bord :: Path,     -- 2 точки, по которым строить линию
                        colB :: Color,    -- Цвет
                        normal :: Point   -- Нормаль
                    } 

data Pic = Pic [Particle] Float [Border] Float

