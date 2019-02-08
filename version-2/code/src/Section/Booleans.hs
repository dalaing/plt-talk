{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Section.Booleans where

import Hedgehog
import qualified Hedgehog.Gen as Gen

import Util.Rules

data Term =
    TmFalse
  | TmTrue
  | TmOr Term Term
  deriving (Eq, Ord, Show)

vFalse :: Rule Term ()
vFalse _ TmFalse = Just ()
vFalse _ _ = Nothing

vTrue :: Rule Term ()
vTrue _ TmTrue = Just ()
vTrue _ _ = Nothing

valueR :: RuleSet Term ()
valueR = mkRuleSet [vFalse, vTrue]

eOr1 :: Rule Term Term
eOr1 step (TmOr tm1 tm2) = do
  tm1' <- step tm1
  pure $ TmOr tm1' tm2
eOr1 _ _ =
  Nothing

eOr2 :: RuleSet Term () -> Rule Term Term
eOr2 value step (TmOr tm1 tm2) = do
  _ <- value tm1
  tm2' <- step tm2
  pure $ TmOr tm1 tm2'
eOr2 _ _ _ =
  Nothing

eOrFalseFalse :: Rule Term Term
eOrFalseFalse _ (TmOr TmFalse TmFalse) =
  Just TmFalse
eOrFalseFalse _ _ =
  Nothing

eOrFalseTrue :: Rule Term Term
eOrFalseTrue _ (TmOr TmFalse TmTrue) =
  Just TmTrue
eOrFalseTrue _ _ =
  Nothing

eOrTrueFalse :: Rule Term Term
eOrTrueFalse _ (TmOr TmTrue TmFalse) =
  Just TmTrue
eOrTrueFalse _ _ =
  Nothing

eOrTrueTrue :: Rule Term Term
eOrTrueTrue _ (TmOr TmTrue TmTrue) =
  Just TmTrue
eOrTrueTrue _ _ =
  Nothing

eOrFalse :: Rule Term Term
eOrFalse _ (TmOr TmFalse tm2) =
  Just tm2
eOrFalse _ _ =
  Nothing

eOrTrue :: Rule Term Term
eOrTrue _ (TmOr TmTrue _) =
  Just TmTrue
eOrTrue _ _ =
  Nothing

evalRulesEager :: [Rule Term Term]
evalRulesEager = [eOr1, eOr2 valueR, eOrFalseFalse, eOrFalseTrue, eOrTrueFalse, eOrTrueTrue]

stepEagerR :: RuleSet Term Term
stepEagerR = mkRuleSet evalRulesEager

evalRulesLazy :: [Rule Term Term]
evalRulesLazy = [eOr1, eOrFalse, eOrTrue]

stepLazyR :: RuleSet Term Term
stepLazyR = mkRuleSet evalRulesLazy

genTerm :: Gen Term
genTerm =
  Gen.recursive Gen.choice
    [ pure TmFalse, pure TmTrue ]
    [ Gen.subterm2 genTerm genTerm TmOr ]

boolRulesEagerDeterminstic :: Property
boolRulesEagerDeterminstic =
  deterministic genTerm evalRulesEager

boolRulesLazyDeterminstic :: Property
boolRulesLazyDeterminstic =
  deterministic genTerm evalRulesLazy

boolRulesEagerExactlyOne :: Property
boolRulesEagerExactlyOne =
  exactlyOne genTerm valueR stepEagerR

boolRulesLazyExactlyOne :: Property
boolRulesLazyExactlyOne =
  exactlyOne genTerm valueR stepLazyR
