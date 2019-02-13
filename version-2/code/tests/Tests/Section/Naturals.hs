{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Tests.Section.Naturals where

import Test.Tasty
import Test.Tasty.Hedgehog

import Section.Naturals

naturalTests :: TestTree
naturalTests =
  testGroup "naturals"
    [ testGroup "eager"
      [ testProperty "value or step" natRulesEagerValueOrStep
      , testProperty "deterministic" natRulesEagerDeterminstic
      ]
    , testGroup "lazy"
      [ testProperty "value or step" natRulesLazyValueOrStep
      , testProperty "deterministic" natRulesLazyDeterminstic
      ]
    ]
