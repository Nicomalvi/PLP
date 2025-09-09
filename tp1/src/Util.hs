module Util where

-- | @alinearDerecha n s@ agrega espacios a la izquierda de @s@ hasta que su longitud sea @n@.
-- Si @s@ ya tiene longitud @>= n@, devuelve @s@.
alinearDerecha :: Int -> String -> String
alinearDerecha n s = if length s >= n then s else replicate (n - (length s)) ' ' ++ s
-- COMENTARIO PARA ESTUDIAR 
-- si tengo menos espacios de los que piden, creo un string de espacios que faltan y lo concateno al principio
-- de s (++ concatena 2 listas y x:xs agrega 1 elemento a una lista)


-- | Dado un índice y una función, actualiza el elemento en la posición del índice
-- aplicando la función al valor actual. Si el índice está fuera de los límites
-- de la lista, devuelve la lista sin cambios.
-- El primer elemento de la lista es el índice 0.
actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem n f xs = zipWith (\original indices -> if indices==n then f original else original) xs (iterate (+1) 0)

-- COMENTARIO PARA ESTUDIAR

-- Iterate genera una lista infinita a partir de una f y un caso base 
-- (En nuestro caso genera una lista infinita de indices; [0, 1, 2, 3...])

-- zipWidth toma 2 listas y crea otra, donde
-- listaNueva[i] = f lista1[i] lista2[i] 

-- Le estamos pasando a zipWidth la lista original y una lista de indices 
-- La funcion hace lo siguiente:

-- listaNueva[i] = listaOriginal[i]         si i =/= n
-- listaNueva[i] = f(listaOriginal[i])      si i == n


-- | infinito positivo (Haskell no tiene literal para +infinito)
infinitoPositivo :: Float
infinitoPositivo = 1 / 0

-- | infinito negativo (Haskell no tiene literal para -infinito)
infinitoNegativo :: Float
infinitoNegativo = -(1 / 0)