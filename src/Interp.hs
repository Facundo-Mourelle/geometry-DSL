module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (Dibujo, foldDib)

-- Gloss provee el tipo Vector y Picture.
type ImagenFlotante = Vector -> Vector -> Vector -> Picture
type Interpretacion a = a -> ImagenFlotante

mitad :: Vector -> Vector
mitad = (0.5 V.*)

-- Interpretaciones de los constructores de Dibujo

--interpreta el operador de rotacion
interp_rotar :: ImagenFlotante -> ImagenFlotante
interp_rotar f d w h = f (d V.+ w) h (V.negate w) 

--interpreta el operador de espejar
interp_espejar :: ImagenFlotante -> ImagenFlotante
interp_espejar f d w h = f (d V.+ w) (V.negate w) h

interp_rotar45 :: ImagenFlotante -> ImagenFlotante
interp_rotar45 f d w h = f (d V.+ new_w) new_w new_h
    where
        new_w = mitad(w V.+ h)
        new_h = mitad(h V.- w)

interp_rotar45_abs :: ImagenFlotante -> ImagenFlotante
interp_rotar45_abs f d w h = f d' w' h'
  where
    centro = d V.+ mitad(w) V.+ mitad(h)
    rot_w  = (1 / sqrt 2) V.* (w V.+ h)
    rot_h  = (1 / sqrt 2) V.* (h V.- w)
    d' = centro V.- mitad(rot_w) V.- mitad(rot_h)
    w' = rot_w
    h' = rot_h

--interpreta el operador de apilar
interp_apilar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_apilar m n f g d w h = pictures [f (d V.+ h') w (r V.* h) , g d w h']
    where
        r' = n/(m+n)
        r = m/(m+n) 
        h' = r' V.* h

        
--interpreta el operador de juntar
interp_juntar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_juntar m n f g d w h = pictures [f d w' h , g (d V.+ w') (r' V.* w) h]
    where
        r' = n/(m+n)
        r = m/(m+n) 
        w' = r V.* w  
    
--interpreta el operador de encimar
interp_encimar :: ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_encimar f g d w h = pictures [f d w h, g d w h]

--interpreta cualquier expresion del tipo Dibujo a
--utilizar foldDib 
interp :: Interpretacion a -> Dibujo a -> ImagenFlotante
interp interp_Base = foldDib 
    interp_Base                 -- Caso base: aplicar la interpretación a figuras básicas
    interp_rotar                -- Rotar
    interp_rotar45              -- Rot45
    interp_rotar45_abs
    interp_espejar              -- Espejar
    interp_apilar               -- Apilar
    interp_juntar               -- Juntar
    interp_encimar              -- Encimar
    
   

