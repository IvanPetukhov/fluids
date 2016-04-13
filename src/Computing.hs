module Computing where
import Types
import Const
import Graphics.Gloss.Interface.Pure.Game


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
               	res | dist > hSm = 0
               	    | otherwise = pMass * wpoly6 dist

-- Суммарная плотность и давление для одной частицы
densSumOne :: Particle -> [Particle] -> Particle
densSumOne (Particle (x,y) v a p d c) ps = changePart
	where
		newDen = sum (map (densForTwo (Particle (x, y) v a p d c)) ps)
		newPress = hardCoef * (newDen - defDen)
		changePart = (Particle (x, y) v a newPress newDen c)

-- Сила давления для 2-х частиц
f_press :: Particle -> Particle -> Point
f_press (Particle (x, y) _ _ p _ _) (Particle (x1, y1) _ _ p1 d1 _) = (fPressX, fPressY)
	where
		distX = (x - x1) / 1000
		distY = (y - y1) / 1000
		dist = sqrt (distX * distX + distY * distY)

		fPressX | ((dist > hSm) || (d1 ==0)) = 0
		        | otherwise = fst (wpoly6GradV (distX, distY) dist) * pMass * (p + p1)  / (2 * d1)
		fPressY | ((dist > hSm) || (d1 ==0)) = 0
		        | otherwise = snd (wpoly6GradV (distX, distY) dist) * pMass * (p + p1)  / (2 * d1) 

-- Сила вязкости для 2-х частиц
f_vis :: Particle -> Particle -> Point
f_vis (Particle (x, y) (vX, vY) _ _ _ _) (Particle (x1, y1) (v1X, v1Y) _ _ d1 _) = (fViscX, fViscY)
     where
     	distX = (x - x1) / 1000
     	distY = (y - y1) / 1000
     	dist = sqrt (distX * distX + distY * distY)
     	fViscX | ((dist > hSm) || (d1 ==0)) = 0
               | otherwise = viscCoef * pMass * (vX - v1X) / d1 * wpoly6DGrad dist
        fViscY | ((dist > hSm) || (d1 ==0)) = 0
               | otherwise = viscCoef * pMass * (vY - v1Y) / d1 * wpoly6DGrad dist
-- Натяжение
f_tensGrad :: Particle -> Particle -> Point
f_tensGrad (Particle (x , y) _ _ _ _ _) (Particle (x1 , y1) _ _ _ dens _) = res
  where
  	distX = (x - x1) / 1000
	distY = (y - y1) / 1000
	dist = sqrt (distX * distX + distY * distY)
	grad = (wpoly6GradV (distX, distY) dist)
	fx | dens == 0 = 0
	   | otherwise = (fst grad) / dens
	fy | dens ==0 = 0
	   | otherwise = (snd grad) / dens
	res = (fx, fy)

-- Сила натяжения коэф
f_tens :: Particle -> Particle ->Float
f_tens (Particle (x, y) _ _ _ _ _) (Particle (x1, y1) _ _ _ dens _) = tens
  where
	distX = (x - x1) / 1000
	distY = (y - y1) / 1000
	tens | dens == 0 = 0
	     | otherwise = (wpoly6DGrad dist) / dens
	dist = sqrt(distX * distX + distY * distY)

 -- Расчет ускорения
acceleration :: Particle -> [Particle] -> Particle
acceleration (Particle (x, y) v a p d c) ps = (Particle (x,y) v accel p d c)
	where
	  part = (Particle (x , y) v a p d c)
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
	  tens =    if (normGrad > 30) then ( - coefTens / normGrad) else 0
	  f_tensX = f_tensSumX * tens * surfTens
	  f_tensY = f_tensSumY * tens * surfTens  
	  accel = (( f_pressX + f_visX + f_tensX + (fst grav)  ) , ( f_pressY + f_visY + f_tensY + (snd grav)  ))

lenVector :: Point -> Point -> Float
lenVector (x1, y1) (x2, y2) = len
	where
		len = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

normalize :: Point -> Point
normalize (x ,y) = normVec
	where
		x1 = x / (lenVector (0, 0) (x, y))
		y1 = y / (lenVector (0, 0) (x, y))
		normVec = (x1, y1)

-- Удары об стену
borderCross :: Particle -> Border ->  Point
borderCross  (Particle (x, y) (vx, vy) _ _ _ _) (Border [(x1, y1), (x2, y2)] _ (nx, ny))  = addAcc
  where
  	midX = (\x y -> if (x == y) then x else (y + x) / 2) x1 x2
  	midY = (\x y -> if (x == y) then x else (y + x) / 2) y1 y2
  	distX = midX - x
  	distY = midY - y
  	dot = nx * distX + ny * distY
  	reboundX = wallEn * (vx * nx + vy * ny) * nx
  	reboundY = wallEn * (vx * nx + vy * ny) * ny
  	addAccX = wallCoef * nx * dot + reboundX
  	addAccY = wallCoef * ny * dot + reboundY
  	condX = (lenVector (x, y) (x1, y1)) <= (lenVector (x1, y1) (x2, y2)) + 10
  	condY = (lenVector (x, y) (x2, y2)) <= (lenVector (x1, y1) (x2, y2)) + 10
  	--condX = (\a b m -> if (a > b) then m >= b && m <= a else m >= a && m <= b) x1 x2 x
  	--condY = (\a b m -> if (a > b) then m >= b && m <= a else m >= a && m <= b) y1 y2 y
  	addAcc = if dot > 0 && dot < 20 && condY && condX then (addAccX , addAccY) else (0 , 0)

-- Удары со всеми стенами
bordersCross :: [Border] -> Particle -> Point
bordersCross bs (Particle (x, y) (vx, vy) (ax, ay) pr den col) = addAcc
  where
  	par = (Particle (x, y) (vx, vy) (ax, ay) pr den col)
  	allBor = map (\x -> borderCross par x) bs
  	addAccX = sum (map (\x -> (fst x)) allBor)
  	addAccY = sum (map (\x -> (snd x)) allBor)
  	addAcc = (ax + addAccX , ay + addAccY)

-- Получение нового положения и новой скорости
newValues :: Particle -> [Border] -> Float-> Particle 
newValues (Particle (x, y) (vx, vy) (ax, ay) pr den col) bs time = (Particle (x1, y1) (vx1, vy1) (ax1, ay1) pr den col)
  where
  	par = (Particle (x, y) (vx, vy) (ax, ay) pr den col)
  	newAcc = bordersCross bs par
  	ax1 = fst newAcc
  	ay1 = snd newAcc
  	x1 = x + vx * time + ax1 * time * time / 2
  	y1 = y + vy * time + ay1 * time * time / 2
  	vx1 = (x1 - x) / time
  	vy1 = (y1 - y) / time

-- Обновление списка частиц
newPar :: [Particle] -> [Border] -> Float -> [Particle]
newPar ps bs time = map (\z -> newValues z bs time) newAcc
  where
    newDen = map (\x -> densSumOne x ps) ps
    newAcc = map (\y -> acceleration y ps) newDen



