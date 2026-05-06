{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Basica.Comun as Comun
import qualified Basica.Ejemplo as Ejemplo
import qualified Basica.Escher as Escher
import Control.Concurrent (forkIO)
import Control.Monad (forever, when)
import Data.IORef
import Data.Maybe (fromMaybe)
import Dibujo
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss.Interface.IO.Display
import Graphics.Gloss.Interface.IO.Game
import Graphics.UI.GLUT.Begin
import Interp
import System.Exit (exitSuccess)
import System.IO

data Estado = Estado
  { dibujoRefList :: [IORef (Dibujo Basica)]
  , zoomRef   :: IORef Float
  , windowSize :: (Int, Int)
  }

default_res :: (Float, Float)
default_res = (1920, 1080)

remove_last :: [a] -> [a]
remove_last [] = []
remove_last xs | length xs == 1 = xs
               | otherwise = init xs

first_elem :: [a] -> [a]
first_elem []     = []
first_elem (x:_)  = [x]

configs :: [Conf]
configs = [ejemploConf, escherFishConf, escherFishHDConf, escherTriConf, vacia]

basic_img :: [Basica]
basic_img = [Triangulo, Cuadrado, FormaF, Fish, FishHD, Empty]

name_basic :: [String]
name_basic = ["Triangulo", "Cuadrado", "FormaF", "Fish", "FishHD", "Empty"]

data Conf = forall a. Conf
  { name :: String,
    basic :: a -> ImagenFlotante,
    fig :: Dibujo a,
    width :: Float,
    height :: Float,
    r :: Picture -> Picture -- Reposicionar figura
  }

escherFishConf :: Conf
escherFishConf =
  Conf
    { name = "escherFishConf",
      basic = Comun.interpBas,
      fig = Escher.escher 3 Fish,
      width = 500,
      height = 500,
      r = id
    }

escherFishHDConf :: Conf
escherFishHDConf =
  Conf
    { name = "escherFishHDConf",
      basic = Comun.interpBas,
      fig = Escher.escher 3 FishHD,
      width = 500,
      height = 500,
      r = id
    }

escherTriConf :: Conf
escherTriConf =
  Conf
    { name = "escherTriConf",
      basic = Comun.interpBas,
      fig = Escher.escher 3 Triangulo,
      width = 500,
      height = 500,
      r = id
    }

ejemploConf :: Conf
ejemploConf =
  Conf
    { name = "ejemploConf",
      basic = Comun.interpBas,
      fig = Ejemplo.ejemplo,
      width = 100,
      height = 100,
      r = id
    }

vacia :: Conf
vacia =
  Conf
    { name = "vacia",
      basic = (\_ -> nada), -- Dibujo vacío
      fig = Figura (blank), -- Figura vacía
      width = 100,
      height = 100,
      r = id
    }

-- Función para obtener el nombre de una imagen base
imagen_to_string :: Basica -> String
imagen_to_string img = fromMaybe "Desconocido" (lookup img (zip basic_img name_basic))

-- Busca en la lista de configuraciones la que tenga el nombre indicado
str_to_config :: String -> Maybe Conf
str_to_config conf_str = lookup conf_str [(name conf, conf) | conf <- configs]

-- Busca en la lista de imágenes la que corresponda con su nombre
str_to_basic :: String -> Basica
str_to_basic conf_str = fromMaybe Empty (lookup conf_str (zip name_basic basic_img))

-- comprender esta función es un buen ejericio.
lineasH :: Vector -> Float -> Float -> [Picture]
lineasH origen@(x, y) longitud separacion = map (lineaH . (* separacion)) [0 ..]
  where
    lineaH h = line [(x, y + h), (x + longitud, y + h)]

-- Una grilla de n líneas, comenzando en origen con una separación de sep y
-- una longitud de l (usamos composición para no aplicar este
-- argumento)
grilla :: Int -> Vector -> Float -> Float -> Picture
grilla n origen sep l = pictures [ls, lsV]
  where
    ls = pictures $ take (n + 1) $ lineasH origen sep l
    lsV = translate 0 (l * toEnum n) (rotate 90 ls)

command_list :: [String]
command_list = ["Espejar", "Rotar", "AbsRotar45", "Apilar", "Juntar", "Ciclar", "Encimar4"]

builtin_display :: IO ()
builtin_display = do
  putStrLn "Elija una de las opciones"
  mapM_ (putStrLn . (" - " ++) . name) configs
  putStrLn "Elija uno o presione 'q' para salir"

  conf <- getLine
  putStrLn (show conf)
  case conf of
    "q" -> exitSuccess
    _ -> do
      putStrLn $ "Seleccionaste la configuración " ++ conf
      case str_to_config conf of
        Just selected_config -> inicial selected_config
        Nothing -> do
          putStrLn "Error: Configuración no encontrada"
          builtin_display

  if conf == "q"
    then exitSuccess
    else do
      putStrLn $ "Seleccionaste la configuración " ++ conf
      case str_to_config conf of
        Just selected_config -> inicial selected_config -- Configuración válida
        Nothing -> do
          putStrLn "Error: Configuración no encontrada"
          builtin_display -- Volver a pregunta

comandos :: [String]
comandos =
  [ "Comandos disponibles:",
    "e (Espejar)",
    "r (Rotar)",
    "w (Rotar45)",
    "t (AbsRotar45)",
    "a (Apilar)",
    "j (Juntar)",
    "c (Ciclar)",
    "n (Encimar4)",
    "o (Imagen original)",
    "q (salir)",
    "+ (acercar)",
    "- (alejar)",
    "z (retroceder)"
  ]

change_and_append :: [IORef (Dibujo Basica)] -> (Dibujo Basica -> Dibujo Basica) -> IO [IORef (Dibujo Basica)]
change_and_append [] _ = return []
change_and_append xs f = do
    let lastRef = last xs
    dibujo <- readIORef lastRef
    let nuevoDibujo = f(dibujo)
    newRef <- newIORef nuevoDibujo
    return (xs ++ [newRef])


modificarEstado :: (Dibujo Basica -> Dibujo Basica) -> Estado -> IO Estado
modificarEstado f estado = do
  nuevaLista <- change_and_append (dibujoRefList estado) f
  return estado { dibujoRefList = nuevaLista }

handleEvent :: Event -> Estado -> IO Estado
handleEvent event estado = case event of
  EventKey (Char 'q') Down _ _ -> exitSuccess

  EventKey (Char 'e') Down _ _ -> modificarEstado Espejar estado
  EventKey (Char 'r') Down _ _ -> modificarEstado Rotar estado
  EventKey (Char 'w') Down _ _ -> modificarEstado Rot45 estado
  EventKey (Char 't') Down _ _ -> modificarEstado AbsRot45 estado
  EventKey (Char 'a') Down _ _ -> modificarEstado (\d -> Apilar 1 1 d d) estado
  EventKey (Char 'j') Down _ _ -> modificarEstado (\d -> Juntar 1 1 d d) estado
  EventKey (Char 'c') Down _ _ -> modificarEstado ciclar estado
  EventKey (Char 'n') Down _ _ -> modificarEstado encimar4 estado
  
  EventKey (Char 'z') Down _ _ -> do
    let new_list = remove_last (dibujoRefList estado)
    return estado { dibujoRefList = new_list}

  EventKey (Char 'o') Down _ _ -> do
    let nuevaLista = take 1 (dibujoRefList estado)
    writeIORef (zoomRef estado) 1.0
    return estado { dibujoRefList = nuevaLista }

  EventKey (Char '+') Down _ _ -> modifyIORef (zoomRef estado) (* 1.2) >> return estado
  EventKey (Char '-') Down _ _ -> modifyIORef (zoomRef estado) (/ 1.2) >> return estado

  _ -> return estado


renderComandos :: [String] -> Float -> (Int, Int) -> Picture
renderComandos cmds _ (winW, winH) =
  let offsetX = fromIntegral winW / 2 - 225
      offsetY = fromIntegral winH / 2 - 50
  in Translate offsetX offsetY $
       Pictures $
         zipWith
           (\i str -> Translate 0 (-20 * fromIntegral i) (outlinedText str))
           [0 ..]
           cmds

outlinedText :: String -> Picture
outlinedText txt =
  Pictures
    [ offset (-1, -1),
      offset (-1, 1),
      offset (1, -1),
      offset (1, 1),
      offset (0, -1),
      offset (0, 1),
      offset (-1, 0),
      offset (1, 0),
      Color white baseText
    ]
  where
    baseText = Scale 0.125 0.125 $ Text txt
    offset (x, y) = Translate x y $ Color (greyN 0.25) baseText

render :: Estado -> IO Picture
render estado = do
  dib <- readIORef (last(dibujoRefList estado))
  zoom <- readIORef (zoomRef estado)
  let (winW, winH) = windowSize estado

  let size = 250
      halfSize = size / 2
      basePic = interp Comun.interpBas dib (-halfSize, -halfSize) (size, 0) (0, size)
      centeredPic = Translate 0 0 basePic
      zoomedPic = Scale zoom zoom centeredPic
      texto = renderComandos comandos zoom (winW, winH)

  return $ Pictures [zoomedPic, texto]


resolucion_a_tupla :: String -> (Int, Int)
resolucion_a_tupla "" = (1920, 1080)
resolucion_a_tupla str =
  let (anchoStr, altoStr) = break (== 'x') str
   in (read anchoStr, read (tail altoStr))


interactive_display :: IO ()
interactive_display = do
  putStrLn "Elija la imagen básica que quiere usar:"
  mapM_ (putStrLn . (" - " ++) . imagen_to_string) basic_img
  basicIO <- getLine

  if basicIO `elem` map imagen_to_string basic_img
    then do
      putStrLn $ "Seleccionaste la imagen base: " ++ basicIO
      let initial_drawing = str_to_basic basicIO
      putStrLn "Elija la resolución de pantalla\no presione ENTER para default(1920x1080):"
      parsed_resolution <- getLine
      let resolution = resolucion_a_tupla parsed_resolution

      -- Introducción de nueva IORef y display de imagen --
      -- IORef para zoom: 1.0

      dibujoRefOriginal <- newIORef (Figura initial_drawing)
      let first_list = [dibujoRefOriginal]

      zoomRef <- newIORef (1.0 :: Float)
      let initial_state = Estado first_list zoomRef resolution 

      playIO
        (InWindow "Dibujo Interactivo" resolution (0, 0))
        (greyN 0.7)                         -- Color del fondo
        60                                  -- FPS
        initial_state                       -- Elemento que espera modificaciones
        render
        handleEvent
        (\_ state -> return state)
    else do
      putStrLn "Imagen base no encontrada"
      interactive_display                   -- Llamar de nuevo a `interactive_display`

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
inicial :: Conf -> IO ()
inicial (Conf name intBas dib w h r) = display win white . withGrid $ imagen
  where
    ancho = (w, 0)
    alto = (0, h)
    imagen = Translate (-w / 2) (-h / 2) $ interp intBas dib (0, 0) ancho alto
    grillaGris = Translate (-w / 2) (-h / 2) $ color grey $ grilla 10 (0, 0) 100 10
    withGrid p = pictures [p, grillaGris]
    grey = makeColorI 120 120 120 120

win = InWindow "Geometry DSL" (500, 500) (0, 0)

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  loop 
  where
    presentacion = "Elija un modo a ejecutar:"
    interactiva = "Interactiva -> presione i"
    builtin = "Dibujos prediseñados -> presione p"

    loop = do
      putStrLn presentacion
      putStrLn interactiva
      putStrLn builtin
      putStr "Ingrese una opción: " >> hFlush stdout
      option <- getLine
      case option of
        "p" -> builtin_display
        "i" -> interactive_display
        _ -> do
          putStrLn "Error en opción de ejecución"
          loop
