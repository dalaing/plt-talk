{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
module Section.LC where

import Control.Monad (ap)
import Data.Functor.Classes

import Data.Map (Map)
import qualified Data.Map as Map

import Bound
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Util.Rules

data Term a =
    TmVar a
  | TmApp (Term a) (Term a)
  | TmLam String (Scope () Term a)
  | TmFalse
  | TmTrue
  | TmOr (Term a) (Term a)
  | TmZero
  | TmSucc (Term a)
  | TmPred (Term a)
  | TmIsZero (Term a)
  | TmIf (Term a) (Term a) (Term a)
  | TmAdd (Term a) (Term a)
  | TmInt Int

deriveEq1 ''Term
deriveOrd1 ''Term
deriveShow1 ''Term

instance Eq a => Eq (Term a) where (==) = eq1
instance Ord a => Ord (Term a) where compare = compare1
instance Show a => Show (Term a) where showsPrec = showsPrec1

instance Functor Term where
  fmap f (TmVar x) = TmVar (f x)
  fmap f (TmApp tm1 tm2) = TmApp (fmap f tm1) (fmap f tm2)
  fmap f (TmLam v s) = TmLam v (fmap f s)
  fmap _ TmFalse = TmFalse
  fmap _ TmTrue = TmTrue
  fmap f (TmOr tm1 tm2) = TmOr (fmap f tm1) (fmap f tm2)
  fmap _ TmZero = TmZero
  fmap f (TmSucc tm) = TmSucc (fmap f tm)
  fmap f (TmPred tm) = TmPred (fmap f tm)
  fmap f (TmIsZero tm) = TmIsZero (fmap f tm)
  fmap f (TmIf tm1 tm2 tm3) = TmIf (fmap f tm1) (fmap f tm2) (fmap f tm3)
  fmap f (TmAdd tm1 tm2) = TmAdd (fmap f tm1) (fmap f tm2)
  fmap _ (TmInt i) = TmInt i

instance Foldable Term where
  foldMap f (TmVar x) = f x
  foldMap f (TmApp tm1 tm2) = foldMap f tm1 <> foldMap f tm2
  foldMap f (TmLam _ s) = foldMap f s
  foldMap _ TmFalse = mempty
  foldMap _ TmTrue = mempty
  foldMap f (TmOr tm1 tm2) = foldMap f tm1 <> foldMap f tm2
  foldMap _ TmZero = mempty
  foldMap f (TmSucc tm) = foldMap f tm
  foldMap f (TmPred tm) = foldMap f tm
  foldMap f (TmIsZero tm) = foldMap f tm
  foldMap f (TmIf tm1 tm2 tm3) = foldMap f tm1 <> foldMap f tm2 <> foldMap f tm3
  foldMap f (TmAdd tm1 tm2) = foldMap f tm1 <> foldMap f tm2
  foldMap _ (TmInt _) = mempty

instance Traversable Term where
  traverse f (TmVar x) = TmVar <$> f x
  traverse f (TmApp tm1 tm2) = TmApp <$> traverse f tm1 <*> traverse f tm2
  traverse f (TmLam v s) = TmLam v <$> traverse f s
  traverse _ TmFalse = pure TmFalse
  traverse _ TmTrue = pure TmTrue
  traverse f (TmOr tm1 tm2) = TmOr <$> traverse f tm1 <*> traverse f tm2
  traverse _ TmZero = pure TmZero
  traverse f (TmSucc tm) = TmSucc <$> traverse f tm
  traverse f (TmPred tm) = TmPred <$> traverse f tm
  traverse f (TmIsZero tm) = TmIsZero <$> traverse f tm
  traverse f (TmIf tm1 tm2 tm3) = TmIf <$> traverse f tm1 <*> traverse f tm2 <*> traverse f tm3
  traverse f (TmAdd tm1 tm2) = TmAdd <$> traverse f tm1 <*> traverse f tm2
  traverse _ (TmInt i) = pure (TmInt i)

instance Applicative Term where
  pure = return
  (<*>) = ap

instance Monad Term where
  return = TmVar
  TmVar x >>= f = f x
  TmApp tm1 tm2 >>= f = TmApp (tm1 >>= f) (tm2 >>= f)
  TmLam v s >>= f = TmLam v (s >>>= f)
  TmFalse >>= _ = TmFalse
  TmTrue >>= _ = TmTrue
  TmOr tm1 tm2 >>= f = TmOr (tm1 >>= f) (tm2 >>= f)
  TmZero >>= _ = TmZero
  TmSucc tm >>= f = TmSucc (tm >>= f)
  TmPred tm >>= f = TmPred (tm >>= f)
  TmIsZero tm >>= f = TmIsZero (tm >>= f)
  TmIf tm1 tm2 tm3 >>= f = TmIf (tm1 >>= f) (tm2 >>= f) (tm3 >>= f)
  TmAdd tm1 tm2 >>= f = TmAdd (tm1 >>= f) (tm2 >>= f)
  TmInt i >>= _ = TmInt i

lam :: String -> Term String -> Term String
lam v tm = TmLam v (abstract1 v tm)

unLam :: Term String -> Term String
unLam (TmLam v s) = instantiate1 (TmVar v) s
unLam tm = tm

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

vInt :: Rule (Term a) ()
vInt _ (TmInt _) =
  Just ()
vInt _ _ =
  Nothing

vLam :: Rule (Term a) ()
vLam _ (TmLam _ _) =
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
  mkRuleSet [vTrue, vFalse, vZero, vInt, vLam, vSuccEager]

vSuccLazy :: Rule (Term a) ()
vSuccLazy _ (TmSucc tm) =
  pure ()
vSuccLazy _ _ =
  Nothing

valueLazyR :: RuleSet (Term a) ()
valueLazyR =
  mkRuleSet [vTrue, vFalse, vZero, vInt, vLam, vSuccLazy]

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

eAppLam :: Rule (Term a) (Term a)
eAppLam _ (TmApp (TmLam _ s) tm) =
  pure $ instantiate1 tm s
eAppLam _ _ =
  Nothing

eAdd1 :: Rule (Term a) (Term a)
eAdd1 step (TmAdd tm1 tm2) = do
  tm1' <- step tm1
  pure $ TmAdd tm1' tm2
eAdd1 _ _ =
  Nothing

eAdd2 :: Rule (Term a) (Term a)
eAdd2 step (TmAdd tm1@(TmInt _) tm2) = do
  tm2' <- step tm2
  pure $ TmAdd tm1 tm2'
eAdd2 _ _ =
  Nothing

eAddIntInt :: Rule (Term a) (Term a)
eAddIntInt _ (TmAdd (TmInt i1) (TmInt i2)) =
  Just (TmInt (i1 + i2))
eAddIntInt _ _ =
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
  , eAppLam
  , eAdd1
  , eAdd2
  , eAddIntInt
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
  , eAppLam
  , eAdd1
  , eAdd2
  , eAddIntInt
  ]

stepLazyR :: RuleSet (Term a) (Term a)
stepLazyR =
  mkRuleSet evalRulesLazy

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
        Gen.subtermM (genTerm' (v : vs)) $ \tm -> pure $ lam v tm
    ]

lcRulesEagerDeterminstic :: Property
lcRulesEagerDeterminstic =
  deterministic genTerm evalRulesEager

lcRulesLazyDeterminstic :: Property
lcRulesLazyDeterminstic =
  deterministic genTerm evalRulesLazy

lcRulesEagerValueOrStep :: Property
lcRulesEagerValueOrStep =
  exactlyOne genTerm valueEagerR stepEagerR

lcRulesLazyValueOrStep :: Property
lcRulesLazyValueOrStep =
  exactlyOne genTerm valueLazyR stepLazyR
