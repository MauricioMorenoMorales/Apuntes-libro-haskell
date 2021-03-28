doubleMe :: Num a => a -> a
doubleMe x = x + x
doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber' :: (Num a, Ord a) => a -> a
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

lostNumbers :: [Integer]
lostNumbers = [4,8,15,16,23,42]

indiceDeElemento :: Char
indiceDeElemento = "Steve Buscemi" !! 6

-- ! Metodos de arrays
primerElementoDeUnaHista :: Integer
primerElementoDeUnaHista = head [5,4,3,2,1]

colaDeUnaLista :: [Integer]
colaDeUnaLista = tail [5,4,3,2,1]

ultimoElementoDeUnaLista :: Integer
ultimoElementoDeUnaLista = last [5,4,3,2,1]

todaLaListaMenosElUltimoElemento :: [Integer]
todaLaListaMenosElUltimoElemento = init [5,4,3,2,1]

--Otras propiedades length, null, reverse, take, drop, minimum, sum, product, elem

-- ! Formas de llenar un array
ciclo :: [Integer]
ciclo = take 10 (cycle [1,2,3])
-- [1,2,3,1,2,3,1,2,3,1]

repiteElementos :: [Integer]
repiteElementos = take 10 (repeat 5)
-- [5,5,5,5,5,5,5,5,5,5]


-- ! Maps
mapeoDeLista :: [Integer]
mapeoDeLista = [x*2 | x <- [1..10]]

mapeoConfiltro :: [Integer]
mapeoConfiltro = [x*2 | x <- [1..10], x*2 >= 12]

boomBangs :: Integral a => [a] -> [[Char]]
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

excluirNumeros :: [Integer]
excluirNumeros = [x | x <- [10..20], x /= 13, x /= 15, x /= 19]

todasLasMultiplicacionesPosibles :: [Integer]
todasLasMultiplicacionesPosibles = [x*y | x <- [2,5,10], y <- [8,10,11]]

todasLasMultiplicacionesPosiblesMayoresQue50 :: [Integer]
todasLasMultiplicacionesPosiblesMayoresQue50 = [x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]

sustantivos :: [[Char]]
sustantivos = ["rana","zebra","cabra"]
adjetivos :: [[Char]]
adjetivos = ["perezosa","enfadada","intrigante"]
palabrasAleatorias :: [[Char]]
palabrasAleatorias = [sustantivos ++ " " ++ adjetivos | sustantivos <- sustantivos, adjetivos <- adjetivos]

length' :: Num a => [t] -> a
length' xs = sum [1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

xxs :: [[Integer]]
xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]

xxsSinImpares :: [[Integer]]
xxsSinImpares = [[x | x <- xs, even x] | xs <- xxs]

-- Tuplas
ejemploTupla :: [(Integer, Integer)]
ejemploTupla = [(1,2), (1,2), (6,7)]

-- Error de tupla = [(1,2),(1,2,3), (1,2)] || [(1,2),("uno",2)]
-- Metodos de tupla fst -> devuelve el primer componente. snd -> devuelve el segundo

emparejarTuplas :: [(Integer, [Char])]
emparejarTuplas = zip [1 .. 5] ["uno","dos","tres","cuatro","cinco"]
-- [(1,"uno"),(2,"dos"),(3,"tres"),(4,"cuatro"),(5,"cinco")]

emparejadoLimitado :: [(Integer, [Char])]
emparejadoLimitado = zip [5,3,2,6,2,7,2,5,4,6,6] ["soy","una","tortuga"]
-- [(5,"soy"),(3,"una"),(2,"tortuga")]

emparejadoLazy :: [(Integer, [Char])]
emparejadoLazy = zip [1..] ["manzana", "naranja", "cereza", "mango"]
-- [(1,"manzana"),(2,"naranja"),(3,"cereza"),(4,"mango")]

trianglesExercise :: [(Integer, Integer, Integer)]
trianglesExercise = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
