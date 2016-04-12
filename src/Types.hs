module Types where
import Graphics.Gloss.Interface.Pure.Game

data Particle = Particle 
					{
						position:: Point,
						velocity:: Point,
						acc:: Point,
						pressure:: Float,
						density:: Float,
						col:: Color
				    }

data Border = Border
					{
						bord :: Path,
						colB :: Color,
						normal :: Point
					} 

data Pic = Pic [Particle] [Border]
