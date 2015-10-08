import Graphics.EasyPlot
import System.Random
import Data.List

{-
	inertie :: [(Float, Float)] -> Float
	inertie classe = sum(map (\x -> (dist (bary classe) x)**2 ) classe)
-}

bary :: [(Float, Float)] -> (Float, Float)
bary l = (sum xs / fromIntegral size, sum ys / fromIntegral size)
	where
		xs = map fst l
		ys = map snd l
		size = length l

dist :: (Float, Float) -> (Float, Float) -> Float
dist (x1,y1) (x2,y2) = sqrt ((x1 - x2)**2 + (y1 - y2)**2)

generateSequence :: Int -> Float -> Float -> IO [Float]
generateSequence nb minV maxV = do
	g <- newStdGen
	return (take nb (randomRs (minV,maxV) g))

generatePoints :: Int -> Float -> Float-> IO [(Float, Float)]
generatePoints nb minV maxV = do
	xs <- generateSequence nb minV maxV
	ys <- generateSequence nb minV maxV
	return (zip xs ys)

{-
hMeans :: [[(Float,Float)]] -> [[(Float,Float)]]
hMeans lc = lc
	where barys = map bary lc
-}

getRandom :: Int -> Int -> IO Int
getRandom minV maxV = do
	g <- newStdGen
	return (fst (randomR (minV,maxV) g))

createAssoc :: [a] -> (a->b) -> [(a,b)]
createAssoc l f = map (\x -> (x, f x)) l

getRandomT :: a -> IO Int
getRandomT x = (getRandom 1 3)

moveIOUp :: (a,IO b) -> IO (a,b)
moveIOUp (x,y) = do
	resY <- y
	return (x,resY)

randomPartition :: [a] -> IO [(a, Int)]
randomPartition l = sequence (map moveIOUp (createAssoc l getRandomT))

getPointsOfClass :: Eq b => [(a,b)] -> b -> [a]
getPointsOfClass cs c = map fst (filter (\x -> snd x == c) cs)

getNewClass :: [((Float,Float),Int)] -> (Float,Float) -> ((Float,Float),Int)
getNewClass bs p = (p, snd distClMin)
	where
		distClMin = minimumBy distance distClForBary
		distClForBary = map (\x ->(dist (fst x) p, snd x)) bs
		distance = (\x y -> compare (fst x) (fst y))

refinePartition :: [((Float,Float),Int)] -> [((Float,Float),Int)]
refinePartition lc | newPartition == lc = newPartition
                   | otherwise = refinePartition newPartition
	where
		newPartition = map (\x -> getNewClass barycentres (fst x)) lc
		barycentres = map (\x -> (bary (getPointsOfClass lc x),x)) classes
		classes = nub (map snd lc)
main :: IO Bool
main = do
	values <- generatePoints 100 0 100
	initialPartition <- randomPartition values

	let lastPartition = refinePartition initialPartition

	plot X11 [
		Data2D [Title "Classe 1", Color Blue] [] (getPointsOfClass lastPartition 1),
		Data2D [Title "Classe 2", Color Red] [] (getPointsOfClass lastPartition 2),
 		Data2D [Title "Classe 3", Color Green] [] (getPointsOfClass lastPartition 3),
 		Data2D [Title "Barycentres", Color Yellow] [] (map (\x -> bary (getPointsOfClass lastPartition x)) [1..3])
		]