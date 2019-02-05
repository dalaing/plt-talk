{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Section.Integers where

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

stepR :: RuleSet Term Term
stepR = mkRuleSet [add1, add2 valueR, addInt]

eval :: Term -> Term
eval = iterR stepR
