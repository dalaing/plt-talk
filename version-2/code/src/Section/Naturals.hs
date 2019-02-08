{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Section.Naturals where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

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

valueEagerR :: RuleSet Term ()
valueEagerR =
  mkRuleSet [vZero, vSuccEager]

vSuccLazy :: Rule Term ()
vSuccLazy _ (TmSucc tm) =
  pure ()
vSuccLazy _ _ =
  Nothing

valueLazyR :: RuleSet Term ()
valueLazyR =
  mkRuleSet [vZero, vSuccLazy]

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

evalRulesEager :: [Rule Term Term]
evalRulesEager =
  [eSucc, ePred, ePredZero, ePredSuccEager valueEagerR]

stepEagerR :: RuleSet Term Term
stepEagerR =
  mkRuleSet evalRulesEager

evalRulesLazy :: [Rule Term Term]
evalRulesLazy = [ePred, ePredZero, ePredSuccLazy]

stepLazyR :: RuleSet Term Term
stepLazyR =
  mkRuleSet evalRulesLazy

genTerm :: Gen Term
genTerm =
  Gen.recursive Gen.choice
    [ pure TmZero ]
    [ Gen.subterm genTerm TmSucc
    , Gen.subterm genTerm TmPred
    ]

natRulesEagerDeterminstic :: Property
natRulesEagerDeterminstic =
  deterministic genTerm evalRulesEager

natRulesLazyDeterminstic :: Property
natRulesLazyDeterminstic =
  deterministic genTerm evalRulesLazy

natRulesEagerExactlyOne :: Property
natRulesEagerExactlyOne =
  exactlyOne genTerm valueEagerR stepEagerR

natRulesLazyExactlyOne :: Property
natRulesLazyExactlyOne =
  exactlyOne genTerm valueLazyR stepLazyR
