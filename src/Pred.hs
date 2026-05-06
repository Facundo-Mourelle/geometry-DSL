module Pred where

import Dibujo

type Pred a = a -> Bool

--Para la definiciones de la funciones de este modulo, no pueden utilizar
--pattern-matching, sino alto orden a traves de la funcion foldDib, mapDib 

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por el resultado de llamar a la función indicada por el
-- segundo argumento con dicha figura.
-- Por ejemplo, `cambiar (== Triangulo) (\x -> Rotar (Basica x))` rota
-- todos los triángulos.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar p f d = foldDib
  (\a -> if p a then f a else Figura a) -- Basica
  Rotar -- Rotar
  Rot45 -- Rot45
  AbsRot45 --AbsRotar45
  Espejar -- Espejar
  Apilar -- Apilar
  Juntar -- Juntar
  Encimar -- Encimar
  d
 
-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p f = foldDib
  p -- Basica
  id -- Rotar 
  id -- Rot45
  id -- AbsRot45
  id -- Espejar 
  (\_ _ b1 b2 -> b1 || b2) -- Apilar
  (\_ _ b1 b2 -> b1 || b2) -- Juntar
  (||) -- Encimar
  f

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib p f = foldDib
  p -- Basica
  id -- Rotar
  id -- Rot45
  id -- AbsRot45
  id -- Espejar
  (\_ _ b1 b2 -> b1 && b2) -- Apilar
  (\_ _ b1 b2 -> b1 && b2) -- Juntar
  (&&) -- Encimar
  f

data Chain = Chain { current :: Float, best :: Float }
  deriving (Show, Eq)

-- Hay 4 rotaciones seguidas.
esRot360 :: Pred (Dibujo a)
esRot360 f =
  let result :: Chain
      result = foldDib
        (\_ -> Chain 0 0) -- Basica
        (\chain -> 
          let newCurrent = current chain + 1
          in Chain newCurrent (max newCurrent (best chain))) -- Rotar
        (\chain -> 
          let newCurrent = current chain + 0.5
          in Chain newCurrent (max newCurrent (best chain))) -- Rot45
        (\chain -> 
          let newCurrent = current chain + 0.5
          in Chain newCurrent (max newCurrent (best chain))) -- AbsRot45
        (\chain -> Chain 0 (best chain)) -- Espejar
        (\p q chain1 chain2 -> Chain 0 (max (best chain1) (best chain2))) -- Apilar
        (\p q chain1 chain2 -> Chain 0 (max (best chain1) (best chain2))) -- Juntar
        (\_ _ -> Chain 0 0) -- Encimar
        f
  in best result >= 4

-- Hay 2 espejados seguidos.
esFlip2 :: Pred (Dibujo a)
esFlip2 f =
  let result :: Chain
      result = foldDib
        (\_ -> Chain 0 0) -- Basica
        (\chain -> Chain 0 (best chain)) -- Rotar
        (\chain -> Chain 0 (best chain)) -- Rot45
        (\chain -> Chain 0 (best chain)) -- AbsRot45
        (\chain -> 
          let newCurrent = current chain + 1
          in Chain newCurrent (max newCurrent (best chain))) -- Espejar
        (\_ _ chain1 chain2 -> Chain 0 (max (best chain1) (best chain2))) -- Apilar
        (\_ _ chain1 chain2 -> Chain 0 (max (best chain1) (best chain2))) -- Juntar
        (\_ _ -> Chain 0 0) -- Encimar
        f
  in best result >= 2

data Superfluo = RotacionSuperflua | FlipSuperfluo
  deriving (Show, Eq)

---- Chequea si el dibujo tiene una rotacion superflua
errorRotacion :: Dibujo a -> [Superfluo]
errorRotacion a = 
  if esFlip2 a 
  then [FlipSuperfluo] 
  else []

-- Chequea si el dibujo tiene un flip superfluo
errorFlip :: Dibujo a -> [Superfluo]
errorFlip a =
  if esRot360 a 
  then [RotacionSuperflua]
  else []

-- Aplica todos los chequeos y acumula todos los errores, y
-- sólo devuelve la figura si no hubo ningún error.
checkSuperfluo :: Dibujo a -> Either [Superfluo] (Dibujo a)
checkSuperfluo a =
  let
    superfluos = errorFlip a ++ errorRotacion a
  in 
    if length superfluos > 0
    then Left superfluos
    else Right a


