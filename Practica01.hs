
--Ejercicio 1
distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

--Ejercicio 2
valorAbsoluto :: Int -> Int 
valorAbsoluto x = if x > 0 
    then x 
    else  x * (-1)

--Ejercicio 3 
pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente  (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)

--Ejercicio 4
hipotenusa :: Float -> Float -> Float 
hipotenusa b h = sqrt ((b ** 2) + (h ** 2))

--Ejercicio 5
raices :: Float -> Float -> Float -> (Float, Float)
raices a b c = 
  ((-b + sqrt (b ** 2 - 4 * a * c)) / (2 * a), 
   (-b - sqrt (b ** 2 - 4 * a * c)) / (2 * a))


--Ejercicio 6
areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt (((a + b + c) / 2) * (((a + b + c) / 2 - a) * ((a + b + c) / 2 - b) * ((a + b + c) / 2 - c)))

--Ejercicio 7
esBisiesto :: Int -> Bool
esBisiesto x = x `mod` 4 == 0


--Ejercicio 8
comparador :: Int -> Int -> Int
comparador x y = if (x == y) 
    then 0
    else if (x < y) 
        then -1
        else if (x > y) 
            then 1
            else  000

--Ejercicio 9
maximo :: Int -> Int -> Int -> Int
maximo x y z = if (z > y && z > x ) 
    then z
    else if ( x > y && x > z ) 
        then x
        else if (y > x && y > z) 
            then y 
            else if (x == z)
                then x
                else if (y == x) 
                    then y
                    else if (y == z) 
                        then y
                        else z
--Ejercicio 10
esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente x y z w = if (x > y  &&  y > z && z > w) 
    then True 
    else False 
