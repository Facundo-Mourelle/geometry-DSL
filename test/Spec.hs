{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Pred 
import Dibujo

import qualified SpecInterp  -- assuming SpecInterp exports a function 'spec' of type Spec
import qualified SpecPred

main :: IO ()
main = hspec $ do
  SpecInterp.spec
  SpecPred.spec
