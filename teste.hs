import Control.Applicative
import Control.Monad.Random

type Point = (Float, Float)
type Poly = [Point]

randomScalar :: (RandomGen g) => Rand g Float
randomScalar = getRandomR (-500, 500)

randomPoint :: (RandomGen g) => Rand g Point
randomPoint = (,) <$> randomScalar <*> randomScalar

randomPoly :: (RandomGen g) => Int -> Rand g Poly
randomPoly n = sequence (replicate n randomPoint)
