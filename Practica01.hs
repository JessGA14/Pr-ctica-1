module Tarea where

--Ejercicio 1
distanciaPuntos :: Floating a => (a, a) -> (a, a) -> a
distanciaPuntos (x1, y1) (x2, y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

--Ejercicio 2
valorAbsoluto :: Int -> Int 
valorAbsoluto x = 
    if x > 0 then x 
     else  x * (-1)

--Ejercicio 3 
pendiente :: Floating a => (a, a) -> (a, a) -> a
pendiente  (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)

--Ejercicio 4
hipotenusa :: Float -> Float -> Float 
hipotenusa b h = sqrt ((b ** 2) + (h ** 2))

--Ejercicio 5
raices :: Float -> Float -> Float -> (Float, Float)
raices a b c = 
    let d = sqrt (b ** 2 - 4 * a * c) 
    in ((-b + d) / (2 * a), (-b - d) / (2 * a))

--Ejercicio 6
semiP :: Float -> Float -> Float -> Float
semiP a b c = (a + b + c)/2

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2 

--Ejercicio 7
esBisiesto :: Int -> Bool
esBisiesto x =
    if ( x  `mod` 400 == 0) then
    True
    else if ( x  `mod` 100 == 0) then 
    False 
    else if ( x  `mod` 4 == 0) then
    True 
    else 
    False

--Ejercicio 8
comparador :: Int -> Int -> Int
comparador x y =
    if (x == y) then
        0
    else if (x < y) then
        -1
    else if (x > y) then
        1
    else 
        000

--Ejercicio 9
maximo :: Int -> Int -> Int -> Int
maximo x y z =
    if (z > y && z > x ) then
        z
    else if ( x > y && x > z ) then 
        x
    else if (y > x && y > z) then
        y
    else if (x == z)then 
        x
    else if (y == x) then 
        y
    else if (y == z) then 
        y
    else 
        z
--Ejercicio 10
esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente x y z w = 
    if (x > y  &&  y > z && z > w) then
        True 
    else 
        False 