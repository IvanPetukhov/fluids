module Computing where
import Types
import Const
import Graphics.Gloss.Interface.Pure.Game

import Data.Maybe (mapMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)

coef :: Float
coef = 315.0 / (64.0 * pi * deg)
           where deg = (hSm ^ 9)

wpoly6 :: Float->Float
wpoly6 r | r  >  hSm = 0 
         | otherwise = coef * ((hSm * hSm - r * r) ^ 3)

wpoly6GradV :: Point -> Float -> Point
wpoly6GradV (x, y) r | r > hSm = (0, 0)
                     | otherwise = (x1, y1)
						where 
							x1 = -1.0 * coef * x * 6.0 * ((hSm * hSm - r * r) ^ 2)
							y1 = -1.0 * coef * y * 6.0 * ((hSm * hSm - r * r) ^ 2)


wpoly6Grad :: Float->Float
wpoly6Grad r | r > hSm = 0
             | otherwise = -1.0 * coef * 6.0 * r * ((hSm * hSm - r * r) ^ 2)

wpoly6DGrad :: Float->Float
wpoly6DGrad r | r > hSm= 0
              | otherwise = coef * 6 * (hSm * hSm - r * r) * (4 * r * r - (hSm * hSm - r * r))

-- Плотность для 2-х частиц
densForTwo :: Particle -> Particle -> Float
densForTwo (Particle (x, y) _ _ _ _ _) (Particle (x1, y1) _ _ _ _ _) = res
            where
            	distX = (x - x1) / 1000
            	distY = (y - y1) / 1000
               	dist = sqrt(distX * distX + distY * distY) 
--              res | ((dist > hSm) && (t == t1)) = 0
--                  | (t == t1) = pMass * wpoly6 dist
--                  | otherwise = 0
                res | dist > hSm = 0
                    | otherwise = pMass * wpoly6 dist

-- Суммарная плотность и давление для одной частицы
densSumOne :: Particle -> [Particle] -> Particle
densSumOne (Particle (x,y) v p d c aP t) ps = changePart
	where
		newDen = sum (map (densForTwo (Particle (x, y) v p d c aP t)) ps)
		newPress = hardCoef * (newDen - defDen)
		changePart = (Particle (x, y) v newPress newDen c aP t)

-- Сила давления для 2-х частиц
f_press :: Particle -> Particle -> Point
f_press (Particle (x, y) _ p _ _ _ t) (Particle (x1, y1) _ p1 d1 _ _ t1) = (fPressX, fPressY)
  where
    distX = (x - x1) / 1000
    distY = (y - y1) / 1000
    dist = sqrt (distX * distX + distY * distY)
--    fPressX | (((dist > hSm) || (d1 == 0)) && (t == t1)) = 0
--            | (t == t1) = fst (wpoly6GradV (distX, distY) dist) * pMass * (p + p1)  / (2 * d1)
--            | otherwise = 0
--    fPressY | (((dist > hSm) || (d1 == 0)) && (t == t1)) = 0
--            | (t == t1) = snd (wpoly6GradV (distX, distY) dist) * pMass * (p + p1)  / (2 * d1)
--		        | otherwise = 0
    fPressX | ((dist > hSm) || (d1 == 0)) = 0
            | otherwise = fst (wpoly6GradV (distX, distY) dist) * pMass * (p + p1)  / (2 * d1)
    fPressY | ((dist > hSm) || (d1 == 0)) = 0
            | otherwise = snd (wpoly6GradV (distX, distY) dist) * pMass * (p + p1)  / (2 * d1)

-- Сила вязкости для 2-х частиц
f_vis :: Particle -> Particle -> Point
f_vis (Particle (x, y) (vX, vY) _ _ _ _ t) (Particle (x1, y1) (v1X, v1Y) _ d1 _ _ t1) = (fViscX, fViscY)
  where
    distX = (x - x1) / 1000
    distY = (y - y1) / 1000
    dist = sqrt (distX * distX + distY * distY)
    fViscX | (((dist > hSm) || (d1 ==0)) && (t == t1)) = 0
           | (t == t1) = viscCoef * pMass * (vX - v1X) / d1 * wpoly6DGrad dist
           | otherwise = 0
    fViscY | (((dist > hSm) || (d1 ==0)) && (t == t1)) = 0
           | (t == t1) = viscCoef * pMass * (vY - v1Y) / d1 * wpoly6DGrad dist
           | otherwise = 0
-- Натяжение
f_tensGrad :: Particle -> Particle -> Point
f_tensGrad (Particle (x , y) _ _ _ _ _ t) (Particle (x1 , y1) _ _ dens _ _ t1) = res
  where
    distX = (x - x1) / 1000
    distY = (y - y1) / 1000
    dist = sqrt (distX * distX + distY * distY)
    grad = (wpoly6GradV (distX, distY) dist)
    fx | ((dens == 0) && (t == t1)) = 0
       | (t == t1) = (fst grad) / dens
       | otherwise = 0
    fy | ((dens == 0) && (t == t1)) = 0
       | (t == t1) =  (snd grad) / dens
       | otherwise = 0
    res = (fx, fy)

-- Сила натяжения коэф
f_tens :: Particle -> Particle ->Float
f_tens (Particle (x, y) _ _ _ _ _ t) (Particle (x1, y1) _ _ dens _ _ t1) = tens
  where
    distX = (x - x1) / 1000
    distY = (y - y1) / 1000
    tens | ((dens == 0) && (t == t1)) = 0
         | (t == t1) = (wpoly6DGrad dist) / dens
         | otherwise = 0
    dist = sqrt(distX * distX + distY * distY)

 -- Расчет ускорения
acceleration :: Particle -> [Particle] -> Point
acceleration (Particle (x, y) v p d c aP t) ps = accel
	where
	  part = (Particle (x , y) v p d c aP t)
	  fPress = map ( \x -> f_press part x) ps
	  f_pressX = ( sum ( map (fst) fPress)) * presCoef
	  f_pressY = (sum (map (snd) fPress)) * presCoef
	  f_visc = map ( \y -> f_vis part y) ps 
	  f_visX = ( sum ( map ( \x -> (fst x)) f_visc)) * viscCoef
	  f_visY = ( sum ( map ( \y -> (snd y)) f_visc)) * viscCoef
	  f_tensCoef = map ( \x -> f_tens part x) ps
	  f_tensGr = map ( \x -> f_tensGrad part x) ps
	  f_tensGradX = map (\x -> fst x) f_tensGr
	  f_tensGradY = map (\y -> snd y) f_tensGr
	  f_tensSumX = (sum f_tensGradX) * pMass
	  f_tensSumY = (sum f_tensGradY) * pMass
	  coefTens = (sum f_tensCoef) * pMass
	  normGrad = (sqrt (f_tensSumX * f_tensSumX + f_tensSumY * f_tensSumY))
	  tens =    if (normGrad > 20) then ( - coefTens / normGrad) else 0
	  f_tensX = f_tensSumX * tens * surfTens
	  f_tensY = f_tensSumY * tens * surfTens  
	  accel = ((f_pressX + f_visX + f_tensX + (fst grav)), (f_pressY + f_visY + f_tensY + (snd grav)))

lenVector :: Point -> Point -> Float
lenVector (x1, y1) (x2, y2) = len
	where
		len = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

-- Удары об стену
borderCross :: Float -> Particle -> Border -> Maybe (Particle, Float)
borderCross  time p@(Particle (x, y) (vx, vy) pr den col aP t) (Border [(x1, y1), (x2, y2)] _ (nx, ny))
  | dot * dot1 >= 0 = Nothing
  | otherwise = Just (Particle (xb, yb) (vnX, vnY) pr den col aP t, th1)
  where

    distX = x1 - x
    distY = y1 - y
    dot = nx * distX + ny * distY

    xn = x + vx * time
    yn = y + vy * time
    
    distX1 = x1 - xn
    distY1 = y1 - yn
    dot1 = nx * distX1 + ny * distY1

    ua = ((xn - x) * (y1 - y) - (yn - y) * (x1 - x)) / ((yn - y) * (x2 - x1) - (xn - x) * (y2 - y1))

    xb = x1 + ua * (x2 - x1)
    yb = y1 + ua * (y2 - y1)
    
    th | vy /= 0   = (yb - y) / vy
       | otherwise = (xb - x) / vx
    
    th1 = time - th

    vnX = wallCoef * (vx + 2 * nx * abs vx)
    vnY = wallCoef * (vy + 2 * ny * abs vy)
    
-- Удары со всеми стенами
bordersCross :: Float -> [Border] -> Particle -> (Particle, Float)
bordersCross time bs p = case sortBy (comparing snd) (mapMaybe (borderCross time p) bs) of
  [] -> (p, time)
  ((p', t'):_) -> bordersCross t' bs p'

equ :: Particle -> Particle -> Bool
equ (Particle (x, y) (vx, vy) _ _ _ _ _) (Particle (x1, y1) (vx1, vy1) _ _ _ _ _) = res
  where
    res = (x == x1) && (y == y1) && (vx == vx1) && (vy == vy1)

-- Получение нового положения и новой скорости
newValues :: Particle -> [Particle] -> Float -> Float -> [Border] -> Float-> Particle
newValues (Particle (x, y) (vx, vy) pr den col aP t) ps axp ayp bs time = parf
  where
    par0 = (Particle (x, y) (vx, vy) pr den col aP t)
    ax = fst (acceleration par0 ps)
    ay = snd (acceleration par0 ps)
    ax1 = ax + axp
    ay1 = ay + ayp
    vx1 = vx + ax1 * time
    vy1 = vy + ay1 * time
    par = Particle (x, y) (vx1, vy1) pr den col aP t
    (Particle (x', y') (vx', vy') _ _ _ _ _, time') = bordersCross time bs par
    parf = (Particle (x' + vx' * time', y' + vy' * time') (vx', vy') pr den col aP t)

-- Обновление списка частиц
newPar :: [Particle] -> Float ->[Border] -> Float -> [Particle]
newPar ps a bs time = map (\z -> newValues z ps ax ay bs time) newDen
  where
    newDen = map (\x -> densSumOne x ps) ps
    ax | a == 1 = accPlus
       | a == 3 = - accPlus
       | otherwise = 0 
    ay | a == 2 = accPlus
       | a == 4 = - accPlus
       | otherwise = 0



