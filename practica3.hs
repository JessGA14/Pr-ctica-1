data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

-------------------- EJERCICIO 1 --------------------
longitud :: Arbol a -> Int 
longitud  ArbolVacio = 0
longitud  (Raiz _ izq der ) = 1 + longitud der + longitud izq 

-------------------- EJERCICIO 2 --------------------
profundidad :: Arbol a -> Int 
profundidad ArbolVacio = 0
profundidad (Raiz _ izq der ) = 1 + max (profundidad izq) (profundidad der)

-------------------- EJERCICIO 3 --------------------
ancho :: Arbol a -> Int
ancho ArbolVacio = 0
ancho (Raiz _ ArbolVacio ArbolVacio) = 1
ancho (Raiz _ izq der) = ancho izq + ancho der

-------------------- EJERCICIO 4 --------------------
data Recorrido = InOrder | PreOrder | PosOrder

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio _ = []
recorrido (Raiz x izq der) InOrder   = recorrido izq InOrder ++ [x] ++ recorrido der InOrder
recorrido (Raiz x izq der) PreOrder  = [x] ++ recorrido izq PreOrder ++ recorrido der PreOrder
recorrido (Raiz x izq der) PosOrder  = recorrido izq PosOrder ++ recorrido der PosOrder ++ [x]



-------------------- EJERCICIO 5 --------------------
niveles :: Arbol a -> [[a]]
niveles arbol = recorrerNivel [arbol]

recorrerNivel :: [Arbol a] -> [[a]]
recorrerNivel [] = []
recorrerNivel nodos =
  if null [v | Raiz v _ _ <- nodos]
    then []
    else [v | Raiz v _ _ <- nodos]
         : recorrerNivel (concat [[izq, der] | Raiz _ izq der <- nodos])

-------------------- EJERCICIO 6 --------------------
minimo :: Arbol a -> a
minimo ArbolVacio = error "Árbol vacío"
minimo (Raiz x ArbolVacio _) = x
minimo (Raiz _ izq _) = minimo izq

-------------------- EJERCICIO 7 --------------------
maximo :: Arbol a -> a
maximo ArbolVacio = error "Árbol vacío"
maximo (Raiz x _ ArbolVacio) = x
maximo (Raiz _ _ der) = maximo der


-------------------- EJERCICIO 8 --------------------
eliminar :: Ord a => Arbol a -> a -> Arbol a
eliminar ArbolVacio _ = error "No existe, es árbol vacío"

eliminar (Raiz z ArbolVacio ArbolVacio) x =
  if z == x
    then ArbolVacio
    else Raiz z ArbolVacio ArbolVacio

eliminar (Raiz z ArbolVacio der) x =
  if x < z
    then Raiz z ArbolVacio (eliminar der x)
    else if x > z
      then Raiz z ArbolVacio der
      else der

eliminar (Raiz z izq ArbolVacio) x =
  if x > z
    then Raiz z (eliminar izq x) ArbolVacio
    else if x < z
      then Raiz z izq ArbolVacio
      else izq

eliminar (Raiz z izq der) x =
  if x < z
    then Raiz z (eliminar izq x) der
    else if x > z
      then Raiz z izq (eliminar der x)
      else
        let m = minimo der
        in Raiz m izq (eliminar der m)
