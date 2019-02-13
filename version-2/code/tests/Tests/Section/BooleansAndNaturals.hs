{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Tests.Section.BooleansAndNaturals where

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.ExpectedFailure

import Section.BooleansAndNaturals

booleanAndNaturalTests :: TestTree
booleanAndNaturalTests =
  testGroup "booleans and naturals"
    [ testGroup "eager"
      [ expectFail $ testProperty "value or step" boolNatRulesEagerValueOrStep
      , testProperty "deterministic" boolNatRulesEagerDeterminstic
      , testProperty "progress" $ progress valueEagerR stepEagerR
      , testProperty "preservation" $ preservation stepEagerR
      ]
    , testGroup "lazy"
      [ expectFail $ testProperty "value or step" boolNatRulesLazyValueOrStep
      , testProperty "deterministic" boolNatRulesLazyDeterminstic
      , testProperty "progress" $ progress valueLazyR stepLazyR
      , testProperty "preservation" $ preservation stepLazyR
      ]
    , testGroup "types"
      [ testProperty "check is consistent" checkCorrect
      , testProperty "infer is consistent" inferCorrect
      ]
    ]
