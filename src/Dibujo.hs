module Dibujo where

-- Definir el lenguaje via constructores de tipo
data Dibujo a
  = Figura a
  | Rotar (Dibujo a)
  | Rot45 (Dibujo a)
  | AbsRot45 (Dibujo a)
  | Espejar (Dibujo a)
  | Apilar Float Float (Dibujo a) (Dibujo a)
  | Juntar Float Float (Dibujo a) (Dibujo a)
  | Encimar (Dibujo a) (Dibujo a)
  deriving (Eq, Show)

-- Composición n-veces de una función con sí misma.

comp :: (a -> a) -> Int -> a -> a
comp f n x = iterate f x !! n

-- se puede usar composicion de funciones `.` pero me perdi intentandolo

-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 a = Rotar (Rotar a)

r270 :: Dibujo a -> Dibujo a
r270 a = Rotar (Rotar (Rotar a))

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) a b = Apilar 1 1 a b
infixr 7 .-.
-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) a b = Juntar 1 1 a b
infixr 8 ///
-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) a b = Encimar a b
infixr 6 ^^^
-- Dadas cuatro dibujos las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto uno dos tres cuatro = Apilar 1 1 (Juntar 1 1 uno dos) (Juntar 1 1 tres cuatro)

-- Una dibujo repetido con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 dib = Encimar dib $ Encimar (Rotar dib) $ Encimar (r180 dib) (r270 dib)

-- es igual a = Encimar (Encimar (Encimar (r270 dib) (r180 dib)) (Rotar dib)) dib)
-- Ref: Learn You a Haskell 6.6

-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar dib = cuarteto dib (Rotar dib) (r180 dib) (r270 dib)

-- Transfomar un valor de tipo a como una Basica.
pureDib :: a -> Dibujo a
pureDib dib = Figura dib


-- Lo instanciamos de la clase Functor para usar fmap
instance Functor Dibujo where
    fmap f (Figura x) = Figura (f x)  
    fmap f (Rotar d) = Rotar (fmap f d)  
    fmap f (Rot45 d) = Rot45 (fmap f d)
    fmap f (AbsRot45 d) = AbsRot45 (fmap f d)  
    fmap f (Espejar d) = Espejar (fmap f d)  
    fmap f (Encimar d1 d2) = Encimar (fmap f d1) (fmap f d2)  
    fmap f (Juntar x y d1 d2) = Juntar x y (fmap f d1) (fmap f d2)  
    fmap f (Apilar x y d1 d2) = Apilar x y (fmap f d1) (fmap f d2) 

-- map para nuestro lenguaje.
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f = fmap f 

-- Funcion de fold para Dibujos a
foldDib ::
  (a -> b) ->
  (b -> b) ->
  (b -> b) ->
  (b -> b) ->
  (b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (b -> b -> b) ->
  Dibujo a ->
  b
-- uso case porque no se permite pattern matching
-- Ref: LYAH 4.5
foldDib fig rot rot45 absrot45 esp apil jun enc dib = case dib of
  Figura a -> fig a
  Rotar dibujo -> rot (recursion dibujo)
  Rot45 dibujo -> rot45 (recursion dibujo)
  AbsRot45 dibujo -> absrot45 (recursion dibujo)
  Espejar dibujo -> esp (recursion dibujo)
  Juntar a b dibujo1 dibujo2 -> jun a b (recursion dibujo1) (recursion dibujo2)
  Apilar a b dibujo1 dibujo2 -> apil a b (recursion dibujo1) (recursion dibujo2)
  Encimar dibujo1 dibujo2 -> enc (recursion dibujo1) (recursion dibujo2)
  where
    recursion = foldDib fig rot rot45 absrot45 esp apil jun enc
