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

todasLasMultiplicacionesPosiblesMayoresQue50 = [x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]

sustantivos :: [[Char]]
sustantivos = ["rana","zebra","cabra"]
adjetivos :: [[Char]]
adjetivos = ["perezosa","enfadada","intrigante"]
palabrasAleatorias :: [[Char]]
palabrasAleatorias = [sustantivos ++ " " ++ adjetivos | sustantivos <- sustantivos, adjetivos <- adjetivos]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]

xxsSinImpares = [[x | x <- xs, even x] | xs <- xxs]

