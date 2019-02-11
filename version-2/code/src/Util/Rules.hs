{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Util.Rules where

import Data.Foldable (asum)
import Data.Maybe (isJust)

import Hedgehog
import qualified Hedgehog.Gen as Gen

type RuleSet a b = a -> Maybe b

type Rule a b = RuleSet a b -> a -> Maybe b

mkRuleSet :: [Rule a b] -> RuleSet a b
mkRuleSet rules =
  let
    ruleSet a =
      asum .
      fmap (\f -> f ruleSet a) $
      rules
  in
    ruleSet

iterR :: RuleSet a a -> a -> a
iterR r x = case r x of
  Nothing -> x
  Just x' -> iterR r x'

deterministic :: (Show a, Show b, Eq b)
              => Gen a
              -> [Rule a b]
              -> Property
deterministic ga rs = property $ do
  (a', a1, a2) <- forAll $ do
    a <- ga
    rs1 <- Gen.shuffle rs
    rs2 <- Gen.shuffle rs
    pure (a, mkRuleSet rs1 a, mkRuleSet rs2 a)
  a1 === a2

exactlyOne :: (Show a)
           => Gen a
           -> RuleSet a b
           -> RuleSet a c
           -> Property
exactlyOne ga r1 r2 = property $ do
  a <- forAll ga
  isJust (r1 a) /== isJust (r2 a)
