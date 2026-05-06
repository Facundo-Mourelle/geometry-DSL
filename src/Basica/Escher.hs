module Basica.Escher where
import Graphics.Gloss
import Dibujo
import Interp
import Basica.Comun

-- supongamos que eligen
type Escher = Bool

-- el dibujo u
dibujoU :: Dibujo Basica -> Dibujo Basica
dibujoU p = encimar4 $ Espejar(Rot45 p)

-- el dibujo t
dibujoT :: Dibujo Basica -> Dibujo Basica
dibujoT d =
    let r1 = Espejar (Rot45 d)
        r2 = r270 r1
    in d ^^^ r1 ^^^ r2

-- lado con nivel de detalle
lado :: Int -> Dibujo Basica -> Dibujo Basica
lado 0 p = Figura Empty
lado n p = cuarteto (lado (n - 1) p) (lado (n - 1) p) (Rotar t) t
  where
    t = dibujoT p

-- esquina con nivel de detalle en base a la figura p
esquina :: Int -> Dibujo Basica -> Dibujo Basica
esquina 0 p = Figura Empty
esquina n d = cuarteto (esquina(n-1) d) (lado (n-1) d) (Rotar(lado (n-1) d)) dibU
    where dibU = dibujoU d

-- por suerte no tenemos que poner el tipo!
noneto p q r s t u v w x =
    let qr = Juntar 1 1 q r
        pqr = Juntar 1 2 p qr
        tu = Juntar 1 1 t u
        stu = Juntar 1 2 s tu
        wx = Juntar 1 1 w x
        vwx = Juntar 1 2 v wx
    in Apilar 1 2 pqr (Apilar 1 1 stu vwx)

-- el dibujo de Basica:
escher :: Int -> Basica -> Dibujo Basica
escher n p = noneto e l (r270 e) (Rotar l) u (r270 l) (Rotar e) (r180 l) (r180 e)
  where
    e = esquina n $ Figura p
    l = lado n $ Figura p
    u = dibujoU $ Figura p
