{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Tests.Section.Integers where

import Test.Tasty
import Test.Tasty.Hedgehog

import Section.Integers

integerTests :: TestTree
integerTests =
  testGroup "integers"
    [ testProperty "value or step" intRulesValueOrStep
    , testProperty "deterministic" intRulesDeterminstic
    ]
