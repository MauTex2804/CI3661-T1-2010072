-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Mauricio Teixeira
--  Carnet: 2010072
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--  Se utilizo IA para comprender algunos mensajes de Error.
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
esPalindromo :: String -> Bool
esPalindromo s = limpiar s == reverso (limpiar s)
   where
      limpiar [] = []
      limpiar (c:cs)
         | isAlphaNum c = toLower c : limpiar cs
         | otherwise    = limpiar cs
      
      reverso [] = []
      reverso (x:xs) = reverso xs ++ [x]

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
-- La recorre una sola vez porque la funcion auxiliar procesa cada elemento actualizando
-- el maximo y el minimo en la misma llamada recursiva.
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (x:xs) = Just (auxiliar x x xs)
   where
      auxiliar maximo minimo [] = (maximo, minimo)
      auxiliar maximo minimo (y:ys)
         | y > maximo = auxiliar y minimo ys
         | y < minimo = auxiliar maximo y ys
         | otherwise  = auxiliar maximo minimo ys

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora [a, b, ':', c, d]
   | not (isDigit a && isDigit b && isDigit c && isDigit d) = Left "formato invalido"
   | h > 23 = Left "hora fuera de rango"
   | m > 59 = Left "minutos fuera de rango"
   | otherwise = Right (h, m)
   where
      h = digitToInt a * 10 + digitToInt b
      m = digitToInt c * 10 + digitToInt d
leerHora _ = Left "formato invalido"

duracion :: String -> String -> Either String Int
duracion h1 h2 = case leerHora h1 of
   Left err -> Left err
   Right (h1', m1) -> case leerHora h2 of
      Left err -> Left err
      Right (h2', m2) ->
         let mins1 = h1' * 60 + m1
             mins2 = h2' * 60 + m2
         in if mins2 >= mins1
            then Right (mins2 - mins1)
            else Right ((24 * 60) - mins1 + mins2)

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral umbral xs =
   let (suma, cuenta) = foldr step (0.0, 0 :: Int) xs
   in if cuenta == 0
      then Nothing
      else Just (suma / fromIntegral cuenta)
   where
      step x (s, c)
         | x > umbral = (x + s, c + 1)
         | otherwise  = (s, c)

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo (f, c) =
   [ (f + df, c + dc)
   | (df, dc) <- [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]
   , let nf = f + df
   , let nc = c + dc
   , nf >= 1 && nf <= 8 && nc >= 1 && nc <= 8
   ]

enDosSaltos :: (Int, Int) -> Int
enDosSaltos pos = length (foldr agregarSinRepetir [] todos)
   where
      todos = concat (map movimientosCaballo (movimientosCaballo pos))
      
      agregarSinRepetir x acc
         | x `elem` acc = acc
         | otherwise    = x : acc

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap f = foldr paso []
   where
      paso x acc = f x : acc

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter p = foldr paso []
   where
      paso x acc
         | p x       = x : acc
         | otherwise = acc

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl f z xs = foldr paso id xs z
   where
      paso x g = \acc -> g (f acc x)
