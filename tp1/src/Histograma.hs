-- | Un `Histograma` es una estructura de datos que permite contar cuántos valores hay en cada rango.
-- @vacio n (a, b)@ devuelve un histograma vacío con n+2 casilleros:
--
-- * @(-inf, a)@
-- * @[a, a + tamIntervalo)@
-- * @[a + tamIntervalo, a + 2*tamIntervalo)@
-- * ...
-- * @[b - tamIntervalo, b)@
-- * @[b, +inf)@
--
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma.
module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
    histograma,
    Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    casilleros,
  )
where

import Util

data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- HAY COMENTARIOS PARA ESTUDIAR ----------------------------------------------------------------------------

-- Histograma i t cs (cs tamaño 8)
--    |       |      |       |       |       |       |       |       |
--   -inf     i                                                    +inf

-- Un histograma representa un rango de numeros separados en casilleros
-- Cada casillero tiene valores dentro de un intervalo de numeros (ejemplo, (-inf;3) o [6;7) )
-- El tamaño de cada INTERVALO de los casilleros es el mismo (t); es decir el rango dentro de todos 
-- tiene de la misma distancia.
-- Lo que difiere es la cantidad de números que representamos dentro de cada intervalo.

-- Por ejemplo, un histograma puede tener estos casilleros:
-- (-inf;3) [3;4) [4;5) [5;+inf)
-- El tamaño de cada intervalo vemos que es 1; 4-3 = 5-4 = 1
-- (Otra cuenta para sacar t es la que usamos en el ej. vacio)
-- Pero el casillero del 3;4 puede tener [3.1111, 3.1112, 3.1113, 3.1114, 3.1115]
-- Mientras que el casillero 4;5 puede tener [4.5, 4.6]
-- Por eso es necesario representar la cantidad de elementos de los casilleros con una lista


-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.

-- Idea
-- Nuestro intervalo comienza en l, por lo tanto l = i (el inicio del intervalo de la 2da casilla)
-- (Numero mas grande - Numero mas chico) / Cantidad de casilleros = t = tamaño de cada intervalo
-- El histograma comienza vacio, por lo que hacemos una lista llena de 0s
-- Hay n casilleros, que nos pasan como parametro, y 2 más para representar valores fuera de rango

vacio :: Int -> (Float, Float) -> Histograma
vacio n (l, u) = Histograma l ((u-l)/(fromIntegral n)) (replicate (n+2) 0)


-- | Agrega un valor al histograma.

-- COMENTARIO
-- Hay que deducir a cual intervalo pertenece x, buscar la posicion de ese intervalo en la 
-- lista y sumarle 1 a ese elemento.

-- Idea general (medio raro)
-- Creo una lista con iterate de todos los intervalos, ya que conozco i, t y el tamaño de cs
-- Luego, para agregar x, x ira en el intervalo donde sea mayor o igual al primer elemento y menor estricto al siguiente

-- Por ejemplo, si tengo 7 intervalos, el primero es 3.0 y el tamaño de cada uno es 2.0, puedo crear esta lista:
-- [3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0]
-- y si me piden agregar el 9.23, ya se en cual intervalo entra (el numero 3 si empezamos en 0)

-- Para detectar ese indice hago un foldl, empiezo en 0 y voy aumentando el acumulador de 1 en 1 (mientras subo de indice)
-- si estoy en el intervalo correcto O SI me pasé, no sumo mas (quiero que el acumulador se quede quieto y me diga cual indice es)
-- si me falta para llegar al intervalo correcto, sigo aumentando de a 1

-- Luego a ese indice le sumo 1, hay que recordar que el primer elemento de mis casilleros es el fuera de rango

-- Si no entienden la notacion de funcion lambda adentro del foldl o algo... hora de las guias...
agregar :: Float -> Histograma -> Histograma
agregar x (Histograma i t cs) = (Histograma i t (actualizarElem ((fromIntegral indiceCambiar)+1) (+1) cs))
                              where
                                indiceCambiar = 
                                  foldl 
                                    (\ac actual -> if (x>=actual && x<actual+t) || x<actual then ac+0 else ac+1) -- funcion foldl
                                    0  -- acumulador comienza en 0
                                    (take (length cs) (iterate (+t) i)) -- lista que uso

-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma n r xs = error "COMPLETAR EJERCICIO 5"

-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
data Casillero = Casillero Float Float Int Float
  deriving (Show, Eq)

-- | Mínimo valor del casillero (el límite inferior puede ser @-inf@)
casMinimo :: Casillero -> Float
casMinimo (Casillero m _ _ _) = m

-- | Máximo valor del casillero (el límite superior puede ser @+inf@)
casMaximo :: Casillero -> Float
casMaximo (Casillero _ m _ _) = m

-- | Cantidad de valores en el casillero. Es un entero @>= 0@.
casCantidad :: Casillero -> Int
casCantidad (Casillero _ _ c _) = c

-- | Porcentaje de valores en el casillero respecto al total de valores en el histograma. Va de 0 a 100.
casPorcentaje :: Casillero -> Float
casPorcentaje (Casillero _ _ _ p) = p

-- | Dado un histograma, devuelve la lista de casilleros con sus límites, cantidad y porcentaje.
casilleros :: Histograma -> [Casillero]
casilleros _ = error "COMPLETAR EJERCICIO 6"
