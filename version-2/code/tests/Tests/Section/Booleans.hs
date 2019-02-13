{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Tests.Section.Booleans where

import Test.Tasty
import Test.Tasty.Hedgehog

import Section.Booleans

booleanTests :: TestTree
booleanTests =
  testGroup "booleans"
    [ testGroup "eager"
      [ testProperty "value or step" boolRulesEagerValueOrStep
      , testProperty "deterministic" boolRulesEagerDeterminstic
      ]
    , testGroup "lazy"
      [ testProperty "value or step" boolRulesLazyValueOrStep
      , testProperty "deterministic" boolRulesLazyDeterminstic
      ]
    ]
