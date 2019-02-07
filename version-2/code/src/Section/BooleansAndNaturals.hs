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

eIsZero :: Rule Term Term
eIsZero step (TmIsZero tm) = do
  tm' <- step tm
  pure $ TmIsZero tm'
eIsZero _ _ =
  Nothing

eIsZeroZero :: Rule Term Term
eIsZeroZero _ (TmIsZero TmZero) = do
  pure TmTrue
eIsZeroZero _ _ =
  Nothing

eIsZeroSuccEager :: RuleSet Term () -> Rule Term Term
eIsZeroSuccEager value _ (TmIsZero (TmSucc tm)) = do
  v <- value tm
  pure TmFalse
eIsZeroSuccEager _ _ _ =
  Nothing

eIsZeroSuccLazy :: Rule Term Term
eIsZeroSuccLazy _ (TmIsZero (TmSucc tm)) =
  pure TmFalse
eIsZeroSuccLazy _ _ =
  Nothing

eIf :: Rule Term Term
eIf step (TmIf tm1 tm2 tm3) = do
  tm1' <- step tm1
  pure $ TmIf tm1' tm2 tm3
eIf _ _ =
  Nothing

eIfTrue :: Rule Term Term
eIfTrue _ (TmIf TmTrue tm _) =
  pure tm
eIfTrue _ _ =
  Nothing

eIfFalse :: Rule Term Term
eIfFalse _ (TmIf TmFalse _ tm) =
  pure tm
eIfFalse _ _ =
  Nothing

stepEagerR :: RuleSet Term Term
stepEagerR =
  mkRuleSet [ eOr1
            , eOr2 valueEagerR
            , eOrFalseFalse
            , eOrFalseTrue
            , eOrTrueFalse
            , eOrTrueTrue
            , eSucc
            , ePred
            , ePredZero
            , ePredSuccEager valueEagerR
            , eIsZero
            , eIsZeroZero
            , eIsZeroSuccEager valueEagerR
            , eIf
            , eIfTrue
            , eIfFalse
            ]

stepLazyR :: RuleSet Term Term
stepLazyR =
  mkRuleSet [ eOr1
            , eOrFalse
            , eOrTrue
            , ePred
            , ePredZero
            , ePredSuccLazy
            , eIsZero
            , eIsZeroZero
            , eIsZeroSuccLazy
            , eIf
            , eIfTrue
            , eIfFalse
            ]

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

inferTrue :: Rule Term Type
inferTrue _ TmTrue =
  Just TyBool
inferTrue _ _ =
  Nothing

inferFalse :: Rule Term Type
inferFalse _ TmFalse =
  Just TyBool
inferFalse _ _ =
  Nothing

inferOr :: Rule Term Type
inferOr step (TmOr tm1 tm2) = do
  expect step tm1 TyBool
  expect step tm2 TyBool
  pure TyBool
inferOr _ _ =
  Nothing

inferZero :: Rule Term Type
inferZero _ TmZero =
  Just TyNat
inferZero _ _ =
  Nothing

inferSucc :: Rule Term Type
inferSucc step (TmSucc tm) = do
  expect step tm TyNat
  pure TyNat
inferSucc _ _ =
  Nothing

inferPred :: Rule Term Type
inferPred step (TmPred tm) = do
  expect step tm TyNat
  pure TyNat
inferPred _ _ =
  Nothing

inferIsZero :: Rule Term Type
inferIsZero step (TmIsZero tm) = do
  expect step tm TyNat
  pure TyBool
inferIsZero _ _ =
  Nothing

inferIf :: Rule Term Type
inferIf step (TmIf tm1 tm2 tm3) = do
  expect step tm1 TyBool
  expectEq step tm1 tm2
inferIf _ _ =
  Nothing

infer :: RuleSet Term Type
infer =
  mkRuleSet [inferFalse, inferTrue, inferOr, inferZero, inferSucc, inferPred, inferIsZero, inferIf]

checkTrue :: Rule (Term, Type) ()
checkTrue _ (TmTrue, TyBool) =
  pure ()
checkTrue _ _ =
  Nothing

checkFalse :: Rule (Term, Type) ()
checkFalse _ (TmFalse, TyBool) =
  pure ()
checkFalse _ _ =
  Nothing

checkOr :: Rule (Term, Type) ()
checkOr step (TmOr tm1 tm2, TyBool) = do
  step (tm1, TyBool)
  step (tm2, TyBool)
checkOr _ _ =
  Nothing

checkZero :: Rule (Term, Type) ()
checkZero _ (TmZero, TyNat) =
  pure ()
checkZero _ _ =
  Nothing

checkSucc :: Rule (Term, Type) ()
checkSucc step (TmSucc tm, TyNat) =
  step (tm, TyNat)
checkSucc _ _ =
  Nothing

checkPred :: Rule (Term, Type) ()
checkPred step (TmPred tm, TyNat) =
  step (tm, TyNat)
checkPred _ _ =
  Nothing

checkIsZero :: Rule (Term, Type) ()
checkIsZero step (TmIsZero tm, TyBool) =
  step (tm, TyNat)
checkIsZero _ _ =
  Nothing

checkIf :: Rule (Term, Type) ()
checkIf step (TmIf tm1 tm2 tm3, ty) = do
  step (tm1, TyBool)
  step (tm2, ty)
  step (tm3, ty)
checkIf _ _ =
  Nothing

check :: RuleSet (Term, Type) ()
check =
  mkRuleSet [checkFalse, checkTrue, checkOr, checkZero, checkSucc, checkPred, checkIsZero, checkIf]

