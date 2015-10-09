import Graphics.EasyPlot
import System.Random
import Data.List

{- Inertie d'un ensemble de Point (classe) -}
inertie :: [(Float, Float)] -> Float
inertie classe = sum(map (\x -> (dist (bary classe) x)**2 ) classe)

{- Barycentre (Point) d'un ensemble de Point -}
bary :: [(Float, Float)] -> (Float, Float)
bary l = (sum xs / fromIntegral size, sum ys / fromIntegral size)
	where
		xs = map fst l
		ys = map snd l
		size = length l

{- Distance entre deux Point -}
dist :: (Float, Float) -> (Float, Float) -> Float
dist (x1,y1) (x2,y2) = sqrt ((x1 - x2)**2 + (y1 - y2)**2)

{- Génère une séquence de nb Float randoms dans une borne définie par [minV;maxV] -}
generateSequence :: Int -> Float -> Float -> IO [Float]
generateSequence nb minV maxV = do
	g <- newStdGen
	return (take nb (randomRs (minV,maxV) g))

{- Génère nb Point avec x et y situés dans [minV;maxV] -}
generatePoints :: Int -> Float -> Float-> IO [(Float, Float)]
generatePoints nb minV maxV = do
	xs <- generateSequence nb minV maxV
	ys <- generateSequence nb minV maxV
	return (zip xs ys)

{- Applique l'algorithme de Hmeans sur une partition initiale (diminue l'inertie globale) -}
hMeans :: [[(Float,Float)]] -> [[(Float,Float)]]
hMeans lc = lc
	where barys = map bary lc

{- Retourne un Int random compris entre minV et maxV -}
getRandom :: Int -> Int -> IO Int
getRandom minV maxV = do
	g <- newStdGen
	return (fst (randomR (minV,maxV) g))

{- Retourne les couples (point, association point) générés par une fonction (f) appliquée à une liste de valeurs (l) -}
createAssoc :: [a] -> (a->b) -> [(a,b)]
createAssoc l f = map (\x -> (x, f x)) l

{- Helper permettant d'appeler getRandom dans une map -}
getRandomT :: Int -> a -> IO Int
getRandomT nb _ = getRandom 1 nb

{- Fix permettant de déplacer le IO dans un couple (a, IO b) -}
moveIOUp :: (a,IO b) -> IO (a,b)
moveIOUp (x,y) = do
	resY <- y
	return (x,resY)

{- génère une partition aléatoire depuis une liste -}
randomPartition :: [a] -> Int -> IO [(a, Int)]
randomPartition l nbclasses = sequence (map moveIOUp (createAssoc l (getRandomT nbclasses)))

{- dans une liste d'associations cs, retourne les éléments de cs associés à la classe c -}
getPointsOfClass :: Eq b => [(a,b)] -> b -> [a]
getPointsOfClass cs c = map fst (filter (\x -> snd x == c) cs)

{- retourne le couple (point, classe) à chaque étape de hmeans depuis un point et une liste de (barycentre, classe) -}
getNewClass :: [((Float,Float),Int)] -> (Float,Float) -> ((Float,Float),Int)
getNewClass bs p = (p, snd distClMin)
	where
		distClMin = minimumBy distance distClForBary
		distClForBary = map (\x ->(dist (fst x) p, snd x)) bs
		distance = (\x y -> compare (fst x) (fst y))

{- raffine une partition avec hmeans tant qu'il y a du changement -}
refinePartition :: [((Float,Float),Int)] -> [((Float,Float),Int)]
refinePartition lc | newPartition == lc = newPartition
                   | otherwise = refinePartition newPartition
	where
		newPartition = map (\x -> getNewClass barycentres (fst x)) lc
		barycentres = map (\x -> (bary (getPointsOfClass lc x),x)) classes
		classes = nub (map snd lc)

getGnuPlotData :: Int -> [((Float,Float),Int)] -> IO (Graph2D Float Float)
getGnuPlotData x p = do
	c <- randomColor
	return (Data2D [Title ("Classe " ++ (show x)), (Color c)] [] (getPointsOfClass p x))

randomColor :: IO Color
randomColor = do
	r <- getRandom 0 255
	g <- getRandom 0 255
	b <- getRandom 0 255
	return (RGB r g b)


{- méthode principale du programme -}
main :: IO ()
main = do
	values <- generatePoints 10000 0 100
	let nbclasses = 15
	initialPartition <- randomPartition values nbclasses
	let lastPartition = refinePartition initialPartition
	points <- sequence (map (\x -> getGnuPlotData x lastPartition) [1..nbclasses])
	let allDatas = points ++ barycentres
		where
			classes = nub (map snd lastPartition)
			barycentres = [Data2D [Title "Barycentres", Color Yellow] [] (map (\x -> bary (getPointsOfClass lastPartition x)) classes)]
	plot X11 allDatas
	print "end"