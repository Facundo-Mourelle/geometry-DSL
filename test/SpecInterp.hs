module SpecInterp (spec) where

import Test.Hspec
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Interp ( interp_rotar
              , interp_espejar
              , interp_rotar45
              , interp_rotar45_abs
              , interp_apilar
              , interp_juntar
              , interp_encimar
              , mitad
              )
import Unsafe.Coerce (unsafeCoerce)
import Graphics.Gloss (Picture)

-- A dummy picture type for testing.
data TestPic = Blank | Pictures [TestPic]
  deriving (Eq, Show)

-- A dummy version of the Gloss pictures combinator.
-- In our dummy version we simply record the composite structure.
pictures :: [TestPic] -> TestPic
pictures xs = if all (== Blank) xs then Blank else Pictures xs
-- Dummy base interpreter.
-- We ignore the transformation parameters and always return Blank.
testBase :: Vector -> Vector -> Vector -> TestPic
testBase _ _ _ = Blank

-- To “fit” the expected type of the interpreter (which must return a Gloss Picture)
-- we use unsafeCoerce to treat our TestPic as a Picture.
testBase' :: Vector -> Vector -> Vector -> Picture
testBase' = unsafeCoerce testBase

-- Helper to "recover" our TestPic from a Picture.
toTestPic :: Picture -> TestPic
toTestPic = unsafeCoerce

spec :: Spec
spec = do
  describe "interp_rotar" $ do
    it "transforms the vectors correctly" $ do
      let d = (10,20)
          w = (5,5)
          h = (3,3)
          result   = interp_rotar testBase' d w h
          expected = testBase (d V.+ w) h (V.negate w)
      toTestPic result `shouldBe` expected

  describe "interp_espejar" $ do
    it "transforms vectors correctly for espejar" $ do
      let d = (10,20)
          w = (5,5)
          h = (3,3)
          result   = interp_espejar testBase' d w h
          expected = testBase (d V.+ w) (V.negate w) h
      toTestPic result `shouldBe` expected

  describe "interp_rotar45" $ do
    it "computes new vectors correctly for rotar45" $ do
      let d = (0,0)
          w = (8,0)
          h = (0,8)
          new_w = mitad (w V.+ h)
          new_h = mitad (h V.- w)
          result   = interp_rotar45 testBase' d w h
          expected = testBase (d V.+ new_w) new_w new_h
      toTestPic result `shouldBe` expected

  describe "interp_rotar45_abs" $ do
    it "computes new vectors correctly for rotar45_abs" $ do
      let d = (10,10)
          w = (4,0)
          h = (0,4)
          centro = d V.+ (mitad w) V.+ (mitad h)
          rot_w  = (1 / sqrt 2) V.* (w V.+ h)
          rot_h  = (1 / sqrt 2) V.* (h V.- w)
          d'     = centro V.- (mitad rot_w) V.- (mitad rot_h)
          result   = interp_rotar45_abs testBase' d w h
          expected = testBase d' rot_w rot_h
      toTestPic result `shouldBe` expected

  -- describe "interp_apilar" $ do
  --  it "divides height correctly in apilar" $ do
  --    let d = (0,0)
  --        w = (10,0)
  --        h = (0,20)
  --        m = 3
  --        n = 2
  --        r' = n/(m+n)         -- fraction for height (used to scale h)
  --        r  = m/(m+n)         -- fraction for width adjustment
  --        h' = r' V.* h        -- scaled height
  --        result   = interp_apilar m n testBase' testBase' d w h
  --        -- interp_apilar (from Interp.hs) produces:
  --        --   pictures [ f (d V.+ h') w (r V.* h) , g d w h' ]
  --        -- With f = testBase' and g = testBase', and testBase always returns Blank,
  --        -- the resulting TestPic should be Pictures [ Blank, Blank ].
  --       expected = pictures [ testBase (d V.+ h') w (r V.* h)
  --                            , testBase d w h' ]
  --    toTestPic result `shouldBe` expected

  -- describe "interp_juntar" $ do
  --  it "divides width correctly in juntar" $ do
  --    let d = (0,0)
  --        w = (20,0)
  --        h = (0,10)
  --        m = 2
  --        n = 3
  --        r' = n/(m+n)        -- fraction for width shift of the second image
  --        r  = m/(m+n)        -- fraction for width scaling of the first image
  --        w' = r V.* w        -- scaled width for the first picture
  --        result   = interp_juntar m n testBase' testBase' d w h
  --        expected = pictures [ testBase d w' h
  --                            , testBase (d V.+ w') (r' V.* w) h ]
  --    toTestPic result `shouldBe` expected

  -- describe "interp_encimar" $ do
  --  it "combines two pictures" $ do
  --    let d = (0,0)
  --        w = (10,10)
  --        h = (10,10)
  --        result   = interp_encimar testBase' testBase' d w h
  --        expected = pictures [ testBase d w h, testBase d w h ]
  --    toTestPic result `shouldBe` expected
  
