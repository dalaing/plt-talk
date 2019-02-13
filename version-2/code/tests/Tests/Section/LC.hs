{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Tests.Section.LC where

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.ExpectedFailure

import Section.LC

lcTests :: TestTree
lcTests =
  testGroup "LC"
    [ testGroup "eager"
      [ expectFail $ testProperty "value or step" lcRulesEagerValueOrStep
      , testProperty "deterministic" lcRulesEagerDeterminstic
      ]
    , testGroup "lazy"
      [ expectFail $ testProperty "value or step" lcRulesLazyValueOrStep
      , testProperty "deterministic" lcRulesLazyDeterminstic
      ]
    ]
