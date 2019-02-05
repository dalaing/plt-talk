{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Util.Rules where

import Data.Foldable (asum)

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
