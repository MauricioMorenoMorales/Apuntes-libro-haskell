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

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- ! Patterns
capital :: String -> String
capital "" = "Una cadena vacia ¬¬"
capital all@(x:_) = "La primera letra de " ++ all ++ " es " ++ [x]

-- ! Guardas

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "Tienes infrapeso ¿Eres emo?"
    | bmi <= 25.0 = "Supuestamente eres normal... Espero seas feo"
    | bmi <= 30   = "¡Estás gordo! Pierde algo de peso gordito."
    | otherwise   = "¡Enhorabuena, eres una ballena!"


bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | weight / height ^ 2 <= 18.5 = "Tienes infrapeso ¿Eres emo?"
    | weight / height ^ 2 <= 25.0 = "Supuestamente eres normal... Espero seas feo"
    | weight / height ^ 2 <= 30.0 = "¡Estás gordo! Pierde algo de peso gordito."
    | otherwise                   = "¡Enhorabuena, eres una ballena!"

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b      = GT
    | a == b     = EQ
    | otherwise  = LT

bmiTellBetter :: (RealFloat a) => a -> a -> String
bmiTellBetter weight height
    | bmi <= 18.5 = "Tienes infrapeso ¿Eres emo?"
    | bmi <= 25.0 = "Supuestamente eres normal... Espero seas feo"
    | bmi <= 30.0 = "¡Estás gordo! Pierde algo de peso gordito."
    | otherwise   = "¡Enhorabuena, eres una ballena!"
    where bmi = weight / height ^ 2

bmiTellBetter' :: (RealFloat a) => a -> a -> String
bmiTellBetter' weight height
    | bmi <= skinny = "Tienes infrapeso ¿Eres emo?"
    | bmi <= normal = "Supuestamente eres normal... Espero seas feo"
    | bmi <= fat = "¡Estás gordo! Pierde algo de peso gordito."
    | otherwise   = "¡Enhorabuena, eres una ballena!"
    where   bmi = weight / height ^ 2
            skinny = 18.5
            normal = 25.0
            fat = 30


bmiTellBetter2 :: (RealFloat a) => a -> a -> String
bmiTellBetter2 weight height
    | bmi <= skinny = "Tienes infrapeso ¿Eres emo?"
    | bmi <= normal = "Supuestamente eres normal... Espero seas feo"
    | bmi <= fat = "¡Estás gordo! Pierde algo de peso gordito."
    | otherwise   = "¡Enhorabuena, eres una ballena!"
    where   bmi = weight / height ^ 2
            (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where   (f:_) = firstname
            (l:_) = lastname

calcBmis :: (RealFloat  a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

