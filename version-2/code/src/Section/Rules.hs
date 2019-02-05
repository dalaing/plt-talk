{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Section.Rules where

import Util.Rules

evenZero :: Rule Int ()
evenZero _ x
  | x == 0 = Just ()
  | otherwise = Nothing

evenAddTwo :: Rule Int ()
evenAddTwo e n
  | n >= 2 = e (n - 2)
  | otherwise = Nothing

evenOdd :: RuleSet Int () -> Rule Int ()
evenOdd o _ n
  | n >= 1 = o (n - 1)
  | otherwise = Nothing

oddEven :: RuleSet Int () -> Rule Int ()
oddEven e _ n
  | n >= 1 = e (n - 1)
  | otherwise = Nothing

even1 :: RuleSet Int ()
even1 =
  mkRuleSet [evenZero, evenAddTwo]

even2, odd2 :: RuleSet Int ()
even2 =
  mkRuleSet [evenZero, evenOdd odd2]
odd2 =
  mkRuleSet [oddEven even2]

even3, odd3 :: RuleSet Int ()
even3 =
  mkRuleSet [evenZero, evenAddTwo, evenOdd odd3]
odd3 =
  mkRuleSet [oddEven even3]
