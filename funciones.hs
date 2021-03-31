lucky :: (Integral a) => a -> String
lucky 7 = "El siete de la suerte"
lucky x = "Lo siento, no es tu día de suerte"

sayMe :: (Integral a) => a -> String
sayMe 1 = "Uno"
sayMe 2 = "Dos"
sayMe 3 = "Tres"
sayMe 4 = "Cuatro"
sayMe 5 = "Cinco"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
-- SI introducimos un valor no existente tenemos que crear un patron exaustivo
-- => Exception: tut.hs:(53,0)-(55,21): Non-exhaustive patterns in function charName

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- ! Listas intencionales
xs :: [(Integer, Integer)]
xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
xs' :: [Integer]
xs' = [a+b | (a,b) <- xs]

head' :: [a] -> a
head' [] = error "Pedazo de animal introduciste una lista vacia"
head' (x:_) = x

tellHowManyElements :: (Show a) => [a] -> String
tellHowManyElements []       = "La lista está vacia"
tellHowManyElements (x:[])   = "La lista tiene un elemento: " ++ show x
tellHowManyElements (x:y:[]) = "La lista tiene dos elementos : " ++ show x ++ " y " ++ show y
tellHowManyElements (x:y:_)  = "La lista es larga, los primeros elementos son" ++ show x ++ " y " ++ show y