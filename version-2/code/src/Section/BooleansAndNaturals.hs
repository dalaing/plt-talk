{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Section.BooleansAndNaturals where

import Util.Rules

data Term =
    TmFalse
  | TmTrue
  | TmOr Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  | TmIf Term Term Term
  deriving (Eq, Ord, Show)

vFalse :: Rule Term ()
vFalse _ TmFalse = Just ()
vFalse _ _ = Nothing

vTrue :: Rule Term ()
vTrue _ TmTrue = Just ()
vTrue _ _ = Nothing

vZero :: Rule Term ()
vZero _ TmZero =
  Just ()
vZero _ _ =
  Nothing

vSuccEager :: Rule Term ()
vSuccEager value (TmSucc tm) = do
  _ <- value tm
  pure ()
vSuccEager _ _ =
  Nothing

valueEagerR :: RuleSet Term ()
valueEagerR =
  mkRuleSet [vTrue, vFalse, vZero, vSuccEager]

vSuccLazy :: Rule Term ()
vSuccLazy _ (TmSucc tm) =
  pure ()
vSuccLazy _ _ =
  Nothing

valueLazyR :: RuleSet Term ()
valueLazyR =
  mkRuleSet [vTrue, vFalse, vZero, vSuccLazy]

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

eSucc :: Rule Term Term
eSucc step (TmSucc tm) = do
  tm' <- step tm
  pure $ TmSucc tm'
eSucc _ _ =
  Nothing

ePred :: Rule Term Term
ePred step (TmPred tm) = do
  tm' <- step tm
  pure $ TmPred tm'
ePred _ _ =
  Nothing

ePredZero :: Rule Term Term
ePredZero _ (TmPred TmZero) =
  Just TmZero
ePredZero _ _ =
  Nothing

ePredSuccEager :: RuleSet Term () -> Rule Term Term
ePredSuccEager value _ (TmPred (TmSucc tm)) = do
  _ <- value tm
  pure tm
ePredSuccEager _ _ _ =
  Nothing

ePredSuccLazy :: Rule Term Term
ePredSuccLazy _ (TmPred (TmSucc tm)) =
  Just tm
ePredSuccLazy _ _ =
  Nothing

stepEagerR :: RuleSet Term Term
stepEagerR =
  mkRuleSet [eOr1, eOr2 valueEagerR, eOrFalseFalse, eOrFalseTrue, eOrTrueFalse, eOrTrueTrue, eSucc, ePred, ePredZero, ePredSuccEager valueEagerR]

stepLazyR :: RuleSet Term Term
stepLazyR =
  mkRuleSet [eOr1, eOrFalse, eOrTrue, ePred, ePredZero, ePredSuccLazy]

data Type =
    TyBool
  | TyNat
  deriving (Eq, Ord, Show)

expect :: Eq ty => RuleSet tm ty -> tm -> ty -> Maybe ()
expect step tm ty = do
  ty' <- step tm
  if (ty == ty')
  then Just ()
  else Nothing

expectEq :: Eq ty => RuleSet tm ty -> tm -> tm -> Maybe ty
expectEq step tm1 tm2 = do
  ty1 <- step tm1
  ty2 <- step tm2
  if (ty1 == ty2)
  then Just ty1
  else Nothing

tTrue :: Rule Term Type
tTrue _ TmTrue =
  Just TyBool
tTrue _ _ =
  Nothing

tFalse :: Rule Term Type
tFalse _ TmFalse =
  Just TyBool
tFalse _ _ =
  Nothing

tOr :: Rule Term Type
tOr step (TmOr tm1 tm2) = do
  expect step tm1 TyBool
  expect step tm2 TyBool
  pure TyBool
tOr _ _ =
  Nothing

tZero :: Rule Term Type
tZero _ TmZero =
  Just TyNat
tZero _ _ =
  Nothing

tSucc :: Rule Term Type
tSucc step (TmSucc tm) = do
  expect step tm TyNat
  pure TyNat
tSucc _ _ =
  Nothing

tPred :: Rule Term Type
tPred step (TmPred tm) = do
  expect step tm TyNat
  pure TyNat
tPred _ _ =
  Nothing

tIsZero :: Rule Term Type
tIsZero step (TmIsZero tm) = do
  expect step tm TyNat
  pure TyBool
tIsZero _ _ =
  Nothing

tIf :: Rule Term Type
tIf step (TmIf tm1 tm2 tm3) = do
  expect step tm1 TyBool
  expectEq step tm1 tm2

infer :: RuleSet Term Type
infer =
  mkRuleSet [tFalse, tTrue, tOr, tZero, tSucc, tPred, tIsZero, tIf]

cTrue :: Rule (Term, Type) ()
cTrue _ (TmTrue, TyBool) =
  pure ()
cTrue _ _ =
  Nothing

cFalse :: Rule (Term, Type) ()
cFalse _ (TmFalse, TyBool) =
  pure ()
cFalse _ _ =
  Nothing

cOr :: Rule (Term, Type) ()
cOr step (TmOr tm1 tm2, TyBool) = do
  step (tm1, TyBool)
  step (tm2, TyBool)
cOr _ _ =
  Nothing

cZero :: Rule (Term, Type) ()
cZero _ (TmZero, TyNat) =
  pure ()
cZero _ _ =
  Nothing

cSucc :: Rule (Term, Type) ()
cSucc step (TmSucc tm, TyNat) =
  step (tm, TyNat)
cSucc _ _ =
  Nothing

cPred :: Rule (Term, Type) ()
cPred step (TmPred tm, TyNat) =
  step (tm, TyNat)
cPred _ _ =
  Nothing

cIsZero :: Rule (Term, Type) ()
cIsZero step (TmIsZero tm, TyBool) =
  step (tm, TyNat)
cIsZero _ _ =
  Nothing

cIf :: Rule (Term, Type) ()
cIf step (TmIf tm1 tm2 tm3, ty) = do
  step (tm1, TyBool)
  step (tm2, ty)
  step (tm3, ty)
cIf _ _ =
  Nothing

check :: RuleSet (Term, Type) ()
check =
  mkRuleSet [cFalse, cTrue, cOr, cZero, cSucc, cPred, cIsZero, cIf]
