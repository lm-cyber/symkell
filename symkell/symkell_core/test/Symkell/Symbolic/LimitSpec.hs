module Symkell.Symbolic.LimitSpec (spec) where

import qualified Control.Exception as Exception
import Data.Ratio ((%))
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck

import Symkell.Symbolic
import Symkell.Symbolic.Limit

spec :: Spec
spec = do
  describe "Basic Limits" $ do
    it "computes limit of a constant" $ do
      let expr = Number 5
      limit expr "x" (RealValue 0) Bidirectional `shouldBe` Finite 5

    it "computes limit of a variable at a point" $ do
      let expr = Symbol "x"
      limit expr "x" (RealValue 3) Bidirectional `shouldBe` Finite 3
      
    it "computes limit of x as x approaches infinity" $ do
      let expr = Symbol "x"
      limit expr "x" PositiveInfinity Bidirectional `shouldBe` PosInfinity
      limit expr "x" NegativeInfinity Bidirectional `shouldBe` NegInfinity
      
  describe "Polynomial Limits" $ do
    it "computes limit of a polynomial at a finite point" $ do
      -- x^2 + 2x + 1 as x approaches 2 should be 9
      let expr = (Symbol "x" :**: Number 2) :+: (Number 2 :*: Symbol "x") :+: Number 1
      limit expr "x" (RealValue 2) Bidirectional `shouldBe` Finite 9
      
    it "computes limit of a polynomial at infinity" $ do
      -- x^2 + 2x + 1 as x approaches infinity should be infinity
      let expr = (Symbol "x" :**: Number 2) :+: (Number 2 :*: Symbol "x") :+: Number 1
      limit expr "x" PositiveInfinity Bidirectional `shouldBe` PosInfinity
      
  describe "Rational Function Limits" $ do
    it "computes limit of a simple rational function" $ do
      -- 1/x as x approaches 0 from the right is infinity
      let expr = Number 1 :/: Symbol "x"
      limit expr "x" (RealValue 0) FromRight `shouldBe` PosInfinity
      limit expr "x" (RealValue 0) FromLeft `shouldBe` NegInfinity
      
    it "computes limit of a rational function at infinity" $ do
      -- (2x + 1)/(x + 3) as x approaches infinity should be 2
      let expr = (Number 2 :*: Symbol "x" :+: Number 1) :/: (Symbol "x" :+: Number 3)
      limit expr "x" PositiveInfinity Bidirectional `shouldBe` Finite 2
      
  describe "Indeterminate Form Limits" $ do
    it "computes limit for 0/0 form using algebraic simplification" $ do
      -- (x^2 - 1)/(x - 1) as x approaches 1
      let expr = ((Symbol "x" :**: Number 2) :-: Number 1) :/: (Symbol "x" :-: Number 1)
      limit expr "x" (RealValue 1) Bidirectional `shouldBe` Finite 2
      
    it "computes limit for 0/0 form using L'Hôpital's rule" $ do
      -- sin(x)/x as x approaches 0
      let expr = UnaryApply Sin (Symbol "x") :/: Symbol "x"
      limit expr "x" (RealValue 0) Bidirectional `shouldBe` Finite 1
      
    it "computes limit for infinity/infinity form" $ do
      -- (3x^2 + x)/(2x^2 - 1) as x approaches infinity
      let expr = (Number 3 :*: (Symbol "x" :**: Number 2) :+: Symbol "x") :/: 
                (Number 2 :*: (Symbol "x" :**: Number 2) :-: Number 1)
      limit expr "x" PositiveInfinity Bidirectional `shouldBe` Finite (3 % 2)
      
    it "computes limit for 1^infinity form" $ do
      -- (1 + 1/x)^x as x approaches infinity (should be e)
      let expr = ((Number 1 :+: (Number 1 :/: Symbol "x")) :**: Symbol "x")
      result <- Exception.evaluate $ limit expr "x" PositiveInfinity Bidirectional
      case result of
        Finite r -> abs (fromRational r - exp 1) < 0.0001 `shouldBe` True
        _ -> expectationFailure "Expected finite result approximately equal to e"
      
  describe "Special Function Limits" $ do
    it "computes limit of exponential functions" $ do
      -- e^x as x approaches infinity
      let expr = UnaryApply Exp (Symbol "x")
      limit expr "x" PositiveInfinity Bidirectional `shouldBe` PosInfinity
      limit expr "x" NegativeInfinity Bidirectional `shouldBe` Finite 0
      
    it "computes limit of logarithmic functions" $ do
      -- log(x) as x approaches 0+
      let expr = UnaryApply Log (Symbol "x")
      limit expr "x" (RealValue 0) FromRight `shouldBe` NegInfinity
      
    it "computes limit of trigonometric functions" $ do
      -- sin(x)/x as x approaches 0
      let expr = UnaryApply Sin (Symbol "x") :/: Symbol "x"
      limit expr "x" (RealValue 0) Bidirectional `shouldBe` Finite 1 
  describe "Basic Limits" $ do
    it "computes limit of a constant" $ do
      let expr = Number 5
      limit expr "x" (RealValue 0) Bidirectional `shouldBe` Finite 5

    it "computes limit of a variable at a point" $ do
      let expr = Symbol "x"
      limit expr "x" (RealValue 3) Bidirectional `shouldBe` Finite 3
      
    it "computes limit of x as x approaches infinity" $ do
      let expr = Symbol "x"
      limit expr "x" PositiveInfinity Bidirectional `shouldBe` PosInfinity
      limit expr "x" NegativeInfinity Bidirectional `shouldBe` NegInfinity
      
  describe "Polynomial Limits" $ do
    it "computes limit of a polynomial at a finite point" $ do
      -- x^2 + 2x + 1 as x approaches 2 should be 9
      let expr = (Symbol "x" :**: Number 2) :+: (Number 2 :*: Symbol "x") :+: Number 1
      limit expr "x" (RealValue 2) Bidirectional `shouldBe` Finite 9
      
    it "computes limit of a polynomial at infinity" $ do
      -- x^2 + 2x + 1 as x approaches infinity should be infinity
      let expr = (Symbol "x" :**: Number 2) :+: (Number 2 :*: Symbol "x") :+: Number 1
      limit expr "x" PositiveInfinity Bidirectional `shouldBe` PosInfinity
      
  describe "Rational Function Limits" $ do
    it "computes limit of a simple rational function" $ do
      -- 1/x as x approaches 0 from the right is infinity
      let expr = Number 1 :/: Symbol "x"
      limit expr "x" (RealValue 0) FromRight `shouldBe` PosInfinity
      limit expr "x" (RealValue 0) FromLeft `shouldBe` NegInfinity
      
    it "computes limit of a rational function at infinity" $ do
      -- (2x + 1)/(x + 3) as x approaches infinity should be 2
      let expr = (Number 2 :*: Symbol "x" :+: Number 1) :/: (Symbol "x" :+: Number 3)
      limit expr "x" PositiveInfinity Bidirectional `shouldBe` Finite 2
      
  describe "Indeterminate Form Limits" $ do
    it "computes limit for 0/0 form using algebraic simplification" $ do
      -- (x^2 - 1)/(x - 1) as x approaches 1
      let expr = ((Symbol "x" :**: Number 2) :-: Number 1) :/: (Symbol "x" :-: Number 1)
      limit expr "x" (RealValue 1) Bidirectional `shouldBe` Finite 2
      
    it "computes limit for 0/0 form using L'Hôpital's rule" $ do
      -- sin(x)/x as x approaches 0
      let expr = UnaryApply Sin (Symbol "x") :/: Symbol "x"
      limit expr "x" (RealValue 0) Bidirectional `shouldBe` Finite 1
      
    it "computes limit for infinity/infinity form" $ do
      -- (3x^2 + x)/(2x^2 - 1) as x approaches infinity
      let expr = (Number 3 :*: (Symbol "x" :**: Number 2) :+: Symbol "x") :/: 
                (Number 2 :*: (Symbol "x" :**: Number 2) :-: Number 1)
      limit expr "x" PositiveInfinity Bidirectional `shouldBe` Finite (3 % 2)
  describe "Basic Limits" $ do
    it "computes limit of a constant" $ do
      let expr = Number 5
      limit expr "x" (RealValue 0) Bidirectional `shouldBe` Finite 5