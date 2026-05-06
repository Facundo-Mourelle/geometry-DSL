# Geometry DSL: Un lenguaje de composición de figuras
Este proyecto implementa un Lenguaje de Dominio Específico (DSL) en Haskell para la composición y generación de dibujos geométricos. Permite combinar figuras básicas mediante transformaciones espaciales (rotaciones, traslaciones, superposiciones) para formar diseños complejos, incluyendo teselados al estilo de M.C. Escher.La visualización gráfica se maneja a través de la librería `gloss`.

## Características Principales
- Composición Declarativa: Creación de figuras complejas mediante combinadores matemáticos.
- Modo Prediseñado: Visualización de configuraciones de prueba (Ejemplo básico, Escher con triángulos, Escher con peces).
- Modo Interactivo: Un entorno de diseño en tiempo real donde el usuario puede aplicar transformaciones, deshacer cambios, hacer zoom y construir figuras dinámicamente mediante atajos de teclado.

## Requisitos e Instalación
Para compilar y ejecutar este proyecto necesitas tener instalado *GHC (Glasgow Haskell Compiler)* y la herramienta de empaquetado `cabal`.
1. Clona el repositorio.
2. Actualiza los repositorios de cabal e instala la librería `gloss`:
```Bash
cabal update
cabal install gloss
```
## Uso y Ejecución
Al ejecutar el programa (dependiendo de tu configuración en el archivo `.cabal`, usualmente mediante `cabal run`), se te presentará un menú en la terminal con dos opciones principales de ejecución:
1. Modo Prediseñado (Presione `p`)
Permite renderizar figuras estáticas ya construidas en el código. Al seleccionar esta opción, el sistema listará las configuraciones disponibles (por ejemplo: `escherFishConf`, `ejemploConf`, etc.). Debes escribir el nombre exacto de la configuración para visualizarla.
2. Modo Interactivo (Presione `i`)
Inicia un lienzo interactivo partiendo de una figura base. Te pedirá:
- Elegir una imagen base (ej. `Triangulo`, `Cuadrado`, `Fish`).
- Definir una resolución (presiona ENTER para usar la resolución por defecto de 1920x1080).
Una vez abierta la ventana de `gloss`, puedes utilizar los siguientes controles de teclado para modificar la figura en tiempo real:
Tecla | Comando | Acción resultante
e | Espejar | Invierte la figura horizontalmente.
r | Rotar | Rota la figura 90 grados.
w | Rotar45 | Rota la figura 45 grados reduciendo su tamaño.
t | AbsRotar45 | Rota 45 grados sin reducir el tamaño de la imagen.
a | Apilar | Pone la figura actual encima de sí misma, dividiendo el espacio.
j | Juntar | Pone la figura actual al lado de sí misma.
c | Ciclar | Crea un cuadrado con la figura rotada i x 90º para i en {0, 1, 2, 3}.
n | Encimar4 | Superpone la figura con sus cuatro rotaciones.
+ / - | Zoom | Acerca (+) o aleja (-) la cámara.
z | Deshacer | Revierte el dibujo a su estado/transformación anterior.
o | Original | Restablece el dibujo a su forma inicial geométrica.
q | Salir | Cierra el programa.
## Estructura del Proyecto y Guía de Desarrollo
Para aquellos interesados en entender o extender el DSL, el código fuente está dividido de la siguiente manera:
- `Main.hs`: Punto de entrada de la aplicación. Maneja el bucle principal, la interacción del usuario (I/O), el estado de `gloss` (IORefs para el historial de dibujo y zoom) y el ruteo de eventos.
- `Dibujo.hs`: Define el tipo de dato algebraico abstracto `<Figura>` y todas las funciones/combinadores de transformación puros.
- `Interp.hs`: Contiene la interpretación matemática y geométrica de las figuras, mapeando el AST del DSL a primitivas gráficas.
- `Basica/Comun.hs`: Interpretaciones base de figuras comunes utilizadas como ejemplos.
- `Basica/Escher.hs`: Definición de combinadores complejos específicos para el diseño de mosaicos y la instanciación de las funciones para generar las teselaciones de M.C. Escher.
## Semántica Formal del DSL
El lenguaje está parametrizado sobre una colección de figuras básicas. La semántica formal de una figura es una función que toma tres vectores *a, b, c* R^2 y produce una figura bidimensional, donde:
- a: indica el vector de desplazamiento del origen.
- b: define el vector de ancho.
- c: define el vector de alto.
Mediante los combinadores proporcionados (como `comp`, `r180`, `(.-.)`, `(///)`, `(^^^)`), se pueden componer programas de tipo `Dibujo a` para producir transformaciones complejas manteniendo la pureza funcional del sistema.
