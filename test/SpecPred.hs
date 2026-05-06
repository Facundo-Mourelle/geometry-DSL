module SpecPred (spec) where

import Test.Hspec
import Pred 
import Dibujo

dFigura :: String -> Dibujo String
dFigura s = Figura s

dRotar :: Dibujo String -> Dibujo String
dRotar = Rotar

dEspejar :: Dibujo String -> Dibujo String
dEspejar = Espejar

spec :: Spec
spec = do
  describe "cambiar" $ do
    it "applies the function only on shapes satisfying the predicate" $ do
      -- For example, change "Triangulo" into a rotated version.
      let dibujo = Figura "Triangulo"
          cambio s = Rotar (Figura s)
          resultado = cambiar (== "Triangulo") cambio dibujo
          esperado  = Rotar (Figura "Triangulo")
      resultado `shouldBe` esperado

    it "leaves shapes that do not satisfy the predicate unchanged" $ do
      let dibujo = Figura "Cuadrado"
          cambio s = Rotar (Figura s)
          resultado = cambiar (== "Triangulo") cambio dibujo
      resultado `shouldBe` Figura "Cuadrado"

  describe "anyDib" $ do
    it "returns True if any basic element satisfies the predicate" $ do
      let dibujo = Figura "Circulo"
      anyDib (== "Circulo") dibujo `shouldBe` True

    it "returns False if no basic element satisfies the predicate" $ do
      let dibujo = Figura "Cuadrado"
      anyDib (== "Circulo") dibujo `shouldBe` False

  describe "allDib" $ do
    it "returns True if all basic elements satisfy the predicate" $ do
      let dibujo = Figura "Circulo"
      allDib (== "Circulo") dibujo `shouldBe` True

    it "returns False if any basic element does not satisfy the predicate" $ do
      let dibujo = Encimar (Figura "Circulo") (Figura "Cuadrado")
      allDib (== "Circulo") dibujo `shouldBe` False

  describe "esRot360" $ do
    it "returns True when there are 4 successive rotations" $ do
      -- Each Rotar adds 1, so 4 rotations should reach 4.
      let dibujo = dRotar (dRotar (dRotar (dRotar (dFigura "A"))))
      esRot360 dibujo `shouldBe` True

    it "returns False when rotations sum to less than 4" $ do
      let dibujo = dRotar (dRotar (dFigura "A"))
      esRot360 dibujo `shouldBe` False

  describe "esFlip2" $ do
    it "returns True when there are 2 successive flips" $ do
      let dibujo = dEspejar (dEspejar (dFigura "A"))
      esFlip2 dibujo `shouldBe` True

    it "returns False when there is only one flip" $ do
      let dibujo = dEspejar (dFigura "A")
      esFlip2 dibujo `shouldBe` False

  describe "errorRotacion and errorFlip" $ do
    it "returns [FlipSuperfluo] when a drawing has two successive flips" $ do
      let dibujo = dEspejar (dEspejar (dFigura "A"))
      errorRotacion dibujo `shouldBe` [FlipSuperfluo]
    
    it "returns [RotacionSuperflua] when a drawing has 4 successive rotations" $ do
      let dibujo = dRotar (dRotar (dRotar (dRotar (dFigura "A"))))
      errorFlip dibujo `shouldBe` [RotacionSuperflua]

  describe "checkSuperfluo" $ do
    it "returns an error (Left) if superfluous operations exist" $ do
      let dibujo = dEspejar (dEspejar (dFigura "A"))
      checkSuperfluo dibujo `shouldBe` Left [FlipSuperfluo]

    it "returns the original drawing (Right) if no errors are found" $ do
      let dibujo = dFigura "A"
      checkSuperfluo dibujo `shouldBe` Right dibujo

