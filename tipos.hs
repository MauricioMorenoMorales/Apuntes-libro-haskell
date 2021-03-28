string1 :: Bool
string1 = ['a','b','c'] == "abc"

-- ! Funciones
--Esta función no es la correcta por que permite recibir datos con punto
factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

-- Esta función solo funciona con numeros pequeños
factorial2 :: Int -> Int
factorial2 n = product [1..n]

-- Esta función funciona bien por que no permite datos Float
factorial3 :: Integer -> Integer
factorial3 n = product [1..n]

-- ! Numeros
-- Este es el mejor para trabajar con decimales
cuadrado :: Double
cuadrado = sqrt 2

cuadrado2 :: Float
cuadrado2  = sqrt 2

-- ! Listas
-- Solo pueden tener un tipo de datos en su contenido
lista :: [Bool]
lista = [True, False, True]

lista2 :: [[Bool]]
lista2 = [[True, False], [True], []]

tupla :: ([Bool], Bool, Char, [Char])
tupla = ([True, False], True, 'a', "ABC")

-- ! Negación

negation :: Bool -> Bool
negation = not

iDontKnowHowThisIsCalled :: Bool -> Bool -> Bool
iDontKnowHowThisIsCalled = (&&)