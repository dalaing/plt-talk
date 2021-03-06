{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
module Section.STLC where

import Control.Monad (ap)
import Data.Functor.Classes
import Data.Maybe (isJust)

import Data.Map (Map)
import qualified Data.Map as Map

import Bound
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Util.Rules

import Debug.Trace

data Type =
    TyBool
  | TyNat
  | TyArr Type Type
  deriving (Eq, Ord, Show)

data Term a =
    TmVar a
  | TmApp (Term a) (Term a)
  | TmLam String Type (Scope () Term a)
  | TmFalse
  | TmTrue
  | TmOr (Term a) (Term a)
  | TmZero
  | TmSucc (Term a)
  | TmPred (Term a)
  | TmIsZero (Term a)
  | TmIf (Term a) (Term a) (Term a)

deriveEq1 ''Term
deriveOrd1 ''Term
deriveShow1 ''Term

instance Eq a => Eq (Term a) where (==) = eq1
instance Ord a => Ord (Term a) where compare = compare1
instance Show a => Show (Term a) where showsPrec = showsPrec1

instance Functor Term where
  fmap f (TmVar x) = TmVar (f x)
  fmap f (TmApp tm1 tm2) = TmApp (fmap f tm1) (fmap f tm2)
  fmap f (TmLam v ty s) = TmLam v ty (fmap f s)
  fmap _ TmFalse = TmFalse
  fmap _ TmTrue = TmTrue
  fmap f (TmOr tm1 tm2) = TmOr (fmap f tm1) (fmap f tm2)
  fmap _ TmZero = TmZero
  fmap f (TmSucc tm) = TmSucc (fmap f tm)
  fmap f (TmPred tm) = TmPred (fmap f tm)
  fmap f (TmIsZero tm) = TmIsZero (fmap f tm)
  fmap f (TmIf tm1 tm2 tm3) = TmIf (fmap f tm1) (fmap f tm2) (fmap f tm3)

instance Foldable Term where
  foldMap f (TmVar x) = f x
  foldMap f (TmApp tm1 tm2) = foldMap f tm1 <> foldMap f tm2
  foldMap f (TmLam _ _ s) = foldMap f s
  foldMap _ TmFalse = mempty
  foldMap _ TmTrue = mempty
  foldMap f (TmOr tm1 tm2) = foldMap f tm1 <> foldMap f tm2
  foldMap _ TmZero = mempty
  foldMap f (TmSucc tm) = foldMap f tm
  foldMap f (TmPred tm) = foldMap f tm
  foldMap f (TmIsZero tm) = foldMap f tm
  foldMap f (TmIf tm1 tm2 tm3) = foldMap f tm1 <> foldMap f tm2 <> foldMap f tm3

instance Traversable Term where
  traverse f (TmVar x) = TmVar <$> f x
  traverse f (TmApp tm1 tm2) = TmApp <$> traverse f tm1 <*> traverse f tm2
  traverse f (TmLam v ty s) = TmLam v ty <$> traverse f s
  traverse _ TmFalse = pure TmFalse
  traverse _ TmTrue = pure TmTrue
  traverse f (TmOr tm1 tm2) = TmOr <$> traverse f tm1 <*> traverse f tm2
  traverse _ TmZero = pure TmZero
  traverse f (TmSucc tm) = TmSucc <$> traverse f tm
  traverse f (TmPred tm) = TmPred <$> traverse f tm
  traverse f (TmIsZero tm) = TmIsZero <$> traverse f tm
  traverse f (TmIf tm1 tm2 tm3) = TmIf <$> traverse f tm1 <*> traverse f tm2 <*> traverse f tm3

instance Applicative Term where
  pure = return
  (<*>) = ap

instance Monad Term where
  return = TmVar
  TmVar x >>= f = f x
  TmApp tm1 tm2 >>= f = TmApp (tm1 >>= f) (tm2 >>= f)
  TmLam v ty s >>= f = TmLam v ty (s >>>= f)
  TmFalse >>= _ = TmFalse
  TmTrue >>= _ = TmTrue
  TmOr tm1 tm2 >>= f = TmOr (tm1 >>= f) (tm2 >>= f)
  TmZero >>= _ = TmZero
  TmSucc tm >>= f = TmSucc (tm >>= f)
  TmPred tm >>= f = TmPred (tm >>= f)
  TmIsZero tm >>= f = TmIsZero (tm >>= f)
  TmIf tm1 tm2 tm3 >>= f = TmIf (tm1 >>= f) (tm2 >>= f) (tm3 >>= f)

lam :: String -> Type -> Term String -> Term String
lam s ty tm = TmLam s ty (abstract1 s tm)

vFalse :: Rule (Term a) ()
vFalse _ TmFalse = Just ()
vFalse _ _ = Nothing

vTrue :: Rule (Term a) ()
vTrue _ TmTrue = Just ()
vTrue _ _ = Nothing

vZero :: Rule (Term a) ()
vZero _ TmZero =
  Just ()
vZero _ _ =
  Nothing

vLam :: Rule (Term a) ()
vLam _ (TmLam _ _ _) =
  Just ()
vLam _ _ =
  Nothing

vSuccEager :: Rule (Term a) ()
vSuccEager value (TmSucc tm) = do
  _ <- value tm
  pure ()
vSuccEager _ _ =
  Nothing

valueEagerR :: RuleSet (Term a) ()
valueEagerR =
  mkRuleSet [vTrue, vFalse, vZero, vLam, vSuccEager]

vSuccLazy :: Rule (Term a) ()
vSuccLazy _ (TmSucc tm) =
  pure ()
vSuccLazy _ _ =
  Nothing

valueLazyR :: RuleSet (Term a) ()
valueLazyR =
  mkRuleSet [vTrue, vFalse, vZero, vLam, vSuccLazy]

eOr1 :: Rule (Term a) (Term a)
eOr1 step (TmOr tm1 tm2) = do
  tm1' <- step tm1
  pure $ TmOr tm1' tm2
eOr1 _ _ =
  Nothing

eOr2 :: RuleSet (Term a) () -> Rule (Term a) (Term a)
eOr2 value step (TmOr tm1 tm2) = do
  _ <- value tm1
  tm2' <- step tm2
  pure $ TmOr tm1 tm2'
eOr2 _ _ _ =
  Nothing

eOrFalseFalse :: Rule (Term a) (Term a)
eOrFalseFalse _ (TmOr TmFalse TmFalse) =
  Just TmFalse
eOrFalseFalse _ _ =
  Nothing

eOrFalseTrue :: Rule (Term a) (Term a)
eOrFalseTrue _ (TmOr TmFalse TmTrue) =
  Just TmTrue
eOrFalseTrue _ _ =
  Nothing

eOrTrueFalse :: Rule (Term a) (Term a)
eOrTrueFalse _ (TmOr TmTrue TmFalse) =
  Just TmTrue
eOrTrueFalse _ _ =
  Nothing

eOrTrueTrue :: Rule (Term a) (Term a)
eOrTrueTrue _ (TmOr TmTrue TmTrue) =
  Just TmTrue
eOrTrueTrue _ _ =
  Nothing

eOrFalse :: Rule (Term a) (Term a)
eOrFalse _ (TmOr TmFalse tm2) =
  Just tm2
eOrFalse _ _ =
  Nothing

eOrTrue :: Rule (Term a) (Term a)
eOrTrue _ (TmOr TmTrue _) =
  Just TmTrue
eOrTrue _ _ =
  Nothing

eSucc :: Rule (Term a) (Term a)
eSucc step (TmSucc tm) = do
  tm' <- step tm
  pure $ TmSucc tm'
eSucc _ _ =
  Nothing

ePred :: Rule (Term a) (Term a)
ePred step (TmPred tm) = do
  tm' <- step tm
  pure $ TmPred tm'
ePred _ _ =
  Nothing

ePredZero :: Rule (Term a) (Term a)
ePredZero _ (TmPred TmZero) =
  Just TmZero
ePredZero _ _ =
  Nothing

ePredSuccEager :: RuleSet (Term a) () -> Rule (Term a) (Term a)
ePredSuccEager value _ (TmPred (TmSucc tm)) = do
  _ <- value tm
  pure tm
ePredSuccEager _ _ _ =
  Nothing

ePredSuccLazy :: Rule (Term a) (Term a)
ePredSuccLazy _ (TmPred (TmSucc tm)) =
  Just tm
ePredSuccLazy _ _ =
  Nothing

eIsZero :: Rule (Term a) (Term a)
eIsZero step (TmIsZero tm) = do
  tm' <- step tm
  pure $ TmIsZero tm'
eIsZero _ _ =
  Nothing

eIsZeroZero :: Rule (Term a) (Term a)
eIsZeroZero _ (TmIsZero TmZero) = do
  pure TmTrue
eIsZeroZero _ _ =
  Nothing

eIsZeroSuccEager :: RuleSet (Term a) () -> Rule (Term a) (Term a)
eIsZeroSuccEager value _ (TmIsZero (TmSucc tm)) = do
  v <- value tm
  pure TmFalse
eIsZeroSuccEager _ _ _ =
  Nothing

eIsZeroSuccLazy :: Rule (Term a) (Term a)
eIsZeroSuccLazy _ (TmIsZero (TmSucc tm)) =
  pure TmFalse
eIsZeroSuccLazy _ _ =
  Nothing

eIf :: Rule (Term a) (Term a)
eIf step (TmIf tm1 tm2 tm3) = do
  tm1' <- step tm1
  pure $ TmIf tm1' tm2 tm3
eIf _ _ =
  Nothing

eIfTrue :: Rule (Term a) (Term a)
eIfTrue _ (TmIf TmTrue tm _) =
  pure tm
eIfTrue _ _ =
  Nothing

eIfFalse :: Rule (Term a) (Term a)
eIfFalse _ (TmIf TmFalse _ tm) =
  pure tm
eIfFalse _ _ =
  Nothing

eApp1 :: Rule (Term a) (Term a)
eApp1 step (TmApp tm1 tm2) = do
  tm1' <- step tm1
  pure $ TmApp tm1' tm2
eApp1 _ _ =
  Nothing

eApp2Eager :: RuleSet (Term a) () -> Rule (Term a) (Term a)
eApp2Eager value step (TmApp tm1 tm2) = do
  _ <- value tm1
  tm2' <- step tm2
  pure $ TmApp tm1 tm2'
eApp2Eager _ _ _ =
  Nothing

eAppLamEager :: RuleSet (Term a) () -> Rule (Term a) (Term a)
eAppLamEager value _ (TmApp (TmLam _ _ s) tm) = do
  _ <- value tm
  pure $ instantiate1 tm s
eAppLamEager _ _ _ =
  Nothing

eAppLamLazy :: Rule (Term a) (Term a)
eAppLamLazy _ (TmApp (TmLam _ _ s) tm) =
  pure $ instantiate1 tm s
eAppLamLazy _ _ =
  Nothing

evalRulesEager :: [Rule (Term a) (Term a)]
evalRulesEager =
  [ eOr1
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
  , eApp1
  , eApp2Eager valueEagerR
  , eAppLamEager valueEagerR
  ]

stepEagerR :: RuleSet (Term a) (Term a)
stepEagerR =
  mkRuleSet evalRulesEager

evalRulesLazy :: [Rule (Term a) (Term a)]
evalRulesLazy =
  [ eOr1
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
  , eApp1
  , eAppLamLazy
  ]

stepLazyR :: RuleSet (Term a) (Term a)
stepLazyR =
  mkRuleSet evalRulesLazy

type Context a = Map a Type

addToContext :: Ord a => a -> Type -> Context a -> Context a
addToContext = Map.insert

fetchFromContext :: Ord a => a -> Context a -> Maybe Type
fetchFromContext = Map.lookup

expect :: Eq ty => RuleSet (ctx, tm) ty -> ctx -> tm -> ty -> Maybe ()
expect step ctx tm ty = do
  ty' <- step (ctx, tm)
  if (ty == ty')
  then Just ()
  else Nothing

expectEq :: Eq ty => RuleSet (ctx, tm) ty -> ctx -> tm -> tm -> Maybe ty
expectEq step ctx tm1 tm2 = do
  ty1 <- step (ctx, tm1)
  ty2 <- step (ctx, tm2)
  if (ty1 == ty2)
  then Just ty1
  else Nothing

inferTrue :: Rule (Context a, Term a) Type
inferTrue _ (_, TmTrue) =
  Just TyBool
inferTrue _ _ =
  Nothing

inferFalse :: Rule (Context a, Term a) Type
inferFalse _ (_, TmFalse) =
  Just TyBool
inferFalse _ _ =
  Nothing

inferOr :: Rule (Context a, Term a) Type
inferOr step (ctx, TmOr tm1 tm2) = do
  expect step ctx tm1 TyBool
  expect step ctx tm2 TyBool
  pure TyBool
inferOr _ _ =
  Nothing

inferZero :: Rule (Context a, Term a) Type
inferZero _ (_, TmZero) =
  Just TyNat
inferZero _ _ =
  Nothing

inferSucc :: Rule (Context a, Term a) Type
inferSucc step (ctx, TmSucc tm) = do
  expect step ctx tm TyNat
  pure TyNat
inferSucc _ _ =
  Nothing

inferPred :: Rule (Context a, Term a) Type
inferPred step (ctx, TmPred tm) = do
  expect step ctx tm TyNat
  pure TyNat
inferPred _ _ =
  Nothing

inferIsZero :: Rule (Context a, Term a) Type
inferIsZero step (ctx, TmIsZero tm) = do
  expect step ctx tm TyNat
  pure TyBool
inferIsZero _ _ =
  Nothing

inferIf :: Rule (Context a, Term a) Type
inferIf step (ctx, TmIf tm1 tm2 tm3) = do
  expect step ctx tm1 TyBool
  expectEq step ctx tm2 tm3
inferIf _ _ =
  Nothing

inferVar :: Ord a => Rule (Context a, Term a) Type
inferVar _ (ctx, TmVar a) =
  fetchFromContext a ctx
inferVar _ _ =
  Nothing

inferLam :: Rule (Context String, Term String) Type
inferLam step (ctx, TmLam v ty s) = do
  rTy <- step (addToContext v ty ctx, instantiate1 (TmVar v) s)
  pure $ TyArr ty rTy
inferLam _ _ =
  Nothing

inferApp :: Rule (Context a, Term a) Type
inferApp step (ctx, TmApp tm1 tm2) = do
  ty1 <- step (ctx, tm1)
  ty2 <- step (ctx, tm2)
  case ty1 of
    TyArr tyF tyT ->
      if ty2 == tyF
      then pure tyT
      else Nothing
    _ -> Nothing
inferApp _ _ =
  Nothing

infer' :: RuleSet (Context String, Term String) Type
infer' =
  mkRuleSet [ inferFalse
            , inferTrue
            , inferOr
            , inferZero
            , inferSucc
            , inferPred
            , inferIsZero
            , inferIf
            , inferVar
            , inferLam
            , inferApp
            ]

infer :: Term String -> Maybe Type
infer tm = infer' (Map.empty, tm)

checkTrue :: Rule (Context a, Term a, Type) ()
checkTrue _ (_, TmTrue, TyBool) =
  pure ()
checkTrue _ _ =
  Nothing

checkFalse :: Rule (Context a, Term a, Type) ()
checkFalse _ (_, TmFalse, TyBool) =
  pure ()
checkFalse _ _ =
  Nothing

checkOr :: Rule (Context a, Term a, Type) ()
checkOr step (ctx, TmOr tm1 tm2, TyBool) = do
  step (ctx, tm1, TyBool)
  step (ctx, tm2, TyBool)
checkOr _ _ =
  Nothing

checkZero :: Rule (Context a, Term a, Type) ()
checkZero _ (_, TmZero, TyNat) =
  pure ()
checkZero _ _ =
  Nothing

checkSucc :: Rule (Context a, Term a, Type) ()
checkSucc step (ctx, TmSucc tm, TyNat) =
  step (ctx, tm, TyNat)
checkSucc _ _ =
  Nothing

checkPred :: Rule (Context a, Term a, Type) ()
checkPred step (ctx, TmPred tm, TyNat) =
  step (ctx, tm, TyNat)
checkPred _ _ =
  Nothing

checkIsZero :: Rule (Context a, Term a, Type) ()
checkIsZero step (ctx, TmIsZero tm, TyBool) =
  step (ctx, tm, TyNat)
checkIsZero _ _ =
  Nothing

checkIf :: Rule (Context a, Term a, Type) ()
checkIf step (ctx, TmIf tm1 tm2 tm3, ty) = do
  step (ctx, tm1, TyBool)
  step (ctx, tm2, ty)
  step (ctx, tm3, ty)
checkIf _ _ =
  Nothing

checkVar :: Ord a => Rule (Context a, Term a, Type) ()
checkVar _ (ctx, TmVar v, ty) = do
  tyC <- fetchFromContext v ctx
  if tyC == ty then pure () else Nothing
checkVar _ _ =
  Nothing

checkLam :: Rule (Context String, Term String, Type) ()
checkLam step (ctx, TmLam v ty s, TyArr tyF tyT) = do
  if ty == tyF
  then do
    step (addToContext v ty ctx, instantiate1 (TmVar v) s, tyT)
  else Nothing
checkLam _ _ =
  Nothing

checkApp :: Rule (Context String, Term String, Type) ()
checkApp step (ctx, TmApp tm1 tm2, ty) = do
  ty1 <- infer' (ctx, tm1)
  case ty1 of
    TyArr tyF tyT -> do
      if tyT == ty
      then step (ctx, tm2, tyF)
      else Nothing
    _ ->
      Nothing
checkApp _ _ =
  Nothing

check' :: RuleSet (Context String, Term String, Type) ()
check' =
  mkRuleSet [ checkFalse
            , checkTrue
            , checkOr
            , checkZero
            , checkSucc
            , checkPred
            , checkIsZero
            , checkIf
            , checkVar
            , checkLam
            , checkApp
            ]

checkType :: Term String -> Type -> Maybe ()
checkType tm ty = check' (Map.empty, tm, ty)

genTerm :: Gen (Term String)
genTerm = genTerm' []

genTerm' :: [String] -> Gen (Term String)
genTerm' vs =
  Gen.recursive Gen.choice
    ((if null vs then id else ((TmVar <$> Gen.element vs) :))
    [ pure TmZero
    , pure TmFalse
    , pure TmTrue
    ])
    [ Gen.subterm (genTerm' vs) TmSucc
    , Gen.subterm (genTerm' vs) TmPred
    , Gen.subterm (genTerm' vs) TmIsZero
    , Gen.subterm2 (genTerm' vs) (genTerm' vs) TmOr
    , Gen.subterm3 (genTerm' vs) (genTerm' vs) (genTerm' vs) TmIf
    , Gen.subterm2 (genTerm' vs) (genTerm' vs) TmApp
    , do
        v <- Gen.filter (not . (`elem` vs)) (pure <$> Gen.alpha)
        ty <- genType
        Gen.subtermM (genTerm' (v : vs)) $ \tm -> pure $ lam v ty tm
    ]

stlcRulesEagerDeterminstic :: Property
stlcRulesEagerDeterminstic =
  deterministic genTerm evalRulesEager

stlcRulesLazyDeterminstic :: Property
stlcRulesLazyDeterminstic =
  deterministic genTerm evalRulesLazy

stlcRulesEagerValueOrStep :: Property
stlcRulesEagerValueOrStep =
  exactlyOne genTerm valueEagerR stepEagerR

stlcRulesLazyValueOrStep :: Property
stlcRulesLazyValueOrStep =
  exactlyOne genTerm valueLazyR stepLazyR

genType :: Gen Type
genType =
  Gen.recursive Gen.choice
  [ pure TyBool, pure TyNat]
  [ Gen.subterm2 genType genType TyArr ]

genTypedTermGeneric :: Context String -> Type -> [Gen (Term String)]
genTypedTermGeneric ctx ty =
  [ Gen.subtermM2 (genTypedTerm' ctx ty) (genTypedTerm' ctx ty) $ \n1 n2 -> do
      b <- genTypedTerm' ctx TyBool
      pure $ TmIf b n1 n2
  , genType >>= \tyF -> Gen.subterm2 (genTypedTerm' ctx (TyArr tyF ty)) (genTypedTerm' ctx tyF) TmApp
  ]

genTypedTerm' :: Context String -> Type -> Gen (Term String)
genTypedTerm' ctx TyBool =
  Gen.recursive Gen.choice
    [ pure TmFalse , pure TmTrue] $
    [ Gen.subterm (genTypedTerm' ctx TyNat) TmIsZero
    , Gen.subterm2 (genTypedTerm' ctx TyBool) (genTypedTerm' ctx TyBool) TmOr
    ] ++ genTypedTermGeneric ctx TyBool
genTypedTerm' ctx TyNat =
  Gen.recursive Gen.choice
    [ pure TmZero ] $
    [ Gen.subterm (genTypedTerm' ctx TyNat) TmSucc
    , Gen.subterm (genTypedTerm' ctx TyNat) TmPred
    ] ++ genTypedTermGeneric ctx TyNat
genTypedTerm' ctx ty@(TyArr ty1 ty2) =
  Gen.recursive Gen.choice
    [ Gen.filter (`Map.notMember` ctx) (pure <$> Gen.alpha) >>= \v ->
        Gen.subterm (genTypedTerm' (addToContext v ty1 ctx) ty2) (lam v ty1) ]
    (genTypedTermGeneric ctx ty)

genTypedTerm :: Type -> Gen (Term String)
genTypedTerm = genTypedTerm' Map.empty

genWellTypedTerm :: Gen (Term String)
genWellTypedTerm =
  genType >>= genTypedTerm

progress :: RuleSet (Term String) () -> RuleSet (Term String) (Term String) -> Property
progress value step = property $ do
  tm <- forAll genWellTypedTerm
  isJust (value tm) /== isJust (step tm)

preservation :: RuleSet (Term String) (Term String) -> Property
preservation step = property $ do
  (ty, tm) <- forAll $ do
    ty' <- genType
    tm' <- Gen.filter (isJust . step) (genTypedTerm ty')
    pure (ty', tm')
  Just ty === (step tm >>= infer)

checkCorrect :: Property
checkCorrect = property $ do
  (ty, tm) <- forAll $ do
    ty' <- genType
    tm' <- genTypedTerm ty'
    pure (ty', tm')
  checkType tm ty === Just ()

inferCorrect :: Property
inferCorrect = property $ do
  (ty, tm) <- forAll $ do
    ty' <- genType
    tm' <- genTypedTerm ty'
    pure (ty', tm')
  infer tm === Just ty
