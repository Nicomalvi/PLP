module Histograma
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
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma
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
import Data.List (zipWith4)
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

-- EJEMPLO
-- agregar (Histograma 0 3 [0,1,2,0,4]) 7.23141328587157 ---> (Histograma 0 3 [0,1,2,1,4])

-- Como lo haria a ojo? me armo una lista de intervalos [a,b) y me voy fijando si x>=a && x < b
-- en este ejemplo tendriamos [-inf ; 0) [0 ; 3) [3 ; 6) [6 ; 9) [9 ; +inf)
-- Noto que 7... es mayor o igual a 6, menor estricto a 9 --> entra en el indice 3
-- hago cs[3] = cs[3]+1 (entra 1 elemento más, cs solo toma cantidades)

-- Recuerdo que ActualizarElem toma un índice (aca seria 3), una función (siempre +1) y una lista (siempre cs)
-- La parte difícil es sacar el índice

-- Iterate toma una función, un x y devuelve una lista infinita de esta forma
-- [x, f(x), f(item anterior), f(item anterior)...]
-- Ejemplo iterate (+1) 0 = [0, 1, 2, 3, 4...]
-- En nuestro caso, iterate +t i = [i, i+t, i+2t, i+3t...] (queda muy parecido a lo que dice el enunciado de Histograma)
-- si cortamos la generacion infinita hasta length cs-2 podemos recrear la lista de intervalos de la siguiente manera
-- [-inf] ++ (take (length cs) (iterate +t i)) ++ [+inf]

-- Acá tendríamos una lista de mínimos de intervalos, falta iterar y descubrir donde entra x

-- Para iterar usamos foldl ya que empieza de la izquierda, podemos comenzar con acumulador en 0
agregar :: Float -> Histograma -> Histograma
agregar x (Histograma i t cs) = (Histograma i t (actualizarElem indice (+1) cs))
                              where
                                indice = 
                                  encontrarIndice
                                    ([infinitoNegativo] ++ (take ((length cs)-2) (iterate (+t) i)) ++ [infinitoPositivo])
                                    x
                                    t
                                
encontrarIndice :: [Float] -> Float -> Float -> Int
encontrarIndice [] y t = 0
encontrarIndice (x:xs) y t = if y>=x+t then (encontrarIndice xs y t) + 1 else 0
--[-inf ; 0) [0 ; 3) [3 ; 6) [6 ; 9) [9 ; +inf)

agregar2 :: Float -> Histograma -> Histograma
agregar2 x (Histograma i t cs) = (Histograma i t (actualizarElem ((fromIntegral indiceCambiar)+1) (+1) cs))
                              where
                                indiceCambiar = 
                                  foldl 
                                    (\ac actual -> if (x >= actual + t) then ac + 1 else ac + 0) 
                                    0
                                    ([infinitoNegativo] ++ (take ((length cs)-2) (iterate (+t) i)) ++ [infinitoPositivo])

-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma n (a, b) xs = 
                        foldl
                          (\hist actual -> agregar actual hist)
                          (vacio n (a, b))
                          xs

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
casilleros (Histograma i t cs) = 
                                zipWith4 crearCasillero cantidades minimos maximos porcentajes
                                  where
                                    cantidades     = cs
                                    minimos        = ([infinitoNegativo] ++ (take ((length cs)- 1) (iterate (+t) i)))
                                    maximos        = ((take ((length cs)- 1) (iterate (+t) i)) ++ [infinitoPositivo])
                                    porcentajes    = (map (\x -> (fromIntegral x)/(fromIntegral (sum cs))) cs)
                                    crearCasillero = (\cantidad minimo maximo porcentaje -> Casillero minimo maximo cantidad porcentaje)
