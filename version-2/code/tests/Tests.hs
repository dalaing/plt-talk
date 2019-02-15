{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Main where

import Test.Tasty

import Tests.Section.Integers
import Tests.Section.Booleans
import Tests.Section.Naturals
import Tests.Section.BooleansAndNaturals
import Tests.Section.LC
import Tests.Section.STLC
import Tests.Section.Pairs

main :: IO ()
main =
  defaultMain $
    testGroup "PLT" [
          integerTests
        , booleanTests
        , naturalTests
        , booleanAndNaturalTests
        , lcTests
        , stlcTests
        , pairTests
        ]
