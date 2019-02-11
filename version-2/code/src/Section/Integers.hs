{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Section.Integers where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Util.Rules

data Term =
    TmInt Int
  | TmAdd Term Term
  deriving (Eq, Ord, Show)

intValue :: Rule Term ()
intValue _ (TmInt _) =
  Just ()
intValue _ _ =
  Nothing

valueR :: RuleSet Term ()
valueR = mkRuleSet [intValue]

add1 :: Rule Term Term
add1 step (TmAdd tm1 tm2) = do
  tm1' <- step tm1
  pure $ TmAdd tm1' tm2
add1 _ _ = Nothing

add2 :: RuleSet Term () -> Rule Term Term
add2 value step (TmAdd tm1 tm2) = do
  _ <- value tm1
  tm2' <- step tm2
  pure $ TmAdd tm1 tm2'
add2 _ _ _ = Nothing

add2' :: Rule Term Term
add2' step (TmAdd tm1@(TmInt _) tm2) = do
  tm2' <- step tm2
  pure $ TmAdd tm1 tm2'
add2' _ _ = Nothing

addInt :: Rule Term Term
addInt _ (TmAdd (TmInt i1) (TmInt i2)) =
  Just $ TmInt (i1 + i2)
addInt _ _ =
  Nothing

evalRules :: [Rule Term Term]
evalRules = [add1, add2 valueR, addInt]

stepR :: RuleSet Term Term
stepR = mkRuleSet evalRules

eval :: Term -> Term
eval = iterR stepR

genTerm :: Gen Term
genTerm =
  Gen.recursive Gen.choice
    [ TmInt <$> Gen.int (Range.linear 0 10) ]
    [ Gen.subterm2 genTerm genTerm TmAdd ]

intRulesDeterminstic :: Property
intRulesDeterminstic =
  deterministic genTerm evalRules

intRulesValueOrStep :: Property
intRulesValueOrStep =
  exactlyOne genTerm valueR stepR
