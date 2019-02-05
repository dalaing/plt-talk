{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Section.Naturals where

import Util.Rules

data Term =
    TmZero
  | TmSucc Term
  | TmPred Term
  deriving (Eq, Ord, Show)

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

vSuccLazy :: Rule Term ()
vSuccLazy _ (TmSucc tm) =
  pure $ TmSucc tm
vSuccLazy _ _ =
  Nothing

valueEagerR :: RuleSet Term ()
valueEagerR = mkRuleSet [vZero, vSuccEager]

valueLazyR :: RuleSet Term ()
valueLazyR = mkRuleSet [vZero, vSuccLazy]

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

ePredSuccEager :: Rule Term () -> Rule Term Term
ePredSuccEager value _ (TmPred (TmSucc tm)) =
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
stepEagerR = mkRuleSet [eSucc, ePred, ePredZero, ePredSuccEager]

stepLazyR :: RuleSet Term Term
stepLazyR = mkRuleSet [ePred, ePredZero, ePredSuccLazy]

