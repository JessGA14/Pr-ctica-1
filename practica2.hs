--Listas con Recursión

-- 1. Obtener la longitud de una lista
longitud :: [a] -> Int 
longitud [] = 0
longitud (x:xs) = 1 + longitud (xs)

-- 2. Sumar todos los números de una lista
sumaLista :: Num a => [a] -> a 
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista(xs)

--3. Agregar un elemento a una lista
agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento lista elemento True  = elemento : lista    
agregaElemento lista elemento False = lista ++ [elemento]

--4. Obtener el máximo de una lista
maximoLista :: (Ord a) => [a] -> a
maximoLista xs = head [x | x <- xs, esMaximo x xs]

esMaximo :: (Ord a) => a -> [a] -> Bool
esMaximo x [] = True
esMaximo x (y:ys)
    | x < y     = False  
    | otherwise = esMaximo x ys 

--5. Recuperar un elemento de una lista de acuerdo a su  índice
recuperarElemento :: [a] -> Int -> a 
recuperarElemento (x:xs) 0 = x
recuperarElemento (x:xs) n 
    | n < 0 = error "ïndice no válido"
    | n >= length (x:xs) = error  "Fuera del rango"
    |otherwise = recuperarElemento xs (n-1)

--Listas por comprehensión.
--1. Recuperar un elemento de una lista de acuerdo a su  índice.
divisoresDeN :: Int -> [Int]
divisoresDeN n
    | n <= 0    = error "El número debe ser positivo"
    | otherwise = [x | x <- [1..n], n `mod` x == 0]


--2. Convertir una lista a conjunto.

conjuntoLista :: Eq a => [a] -> [a]
conjuntoLista [] = []
conjuntoLista (x:xs) = x : [y | y <- conjuntoLista xs, y /= x]



--3. Obtener los números pares de una lista
soloPares :: [Int] -> [Int]
soloPares lista = [x|x <- lista, x `mod` 2 == 0]