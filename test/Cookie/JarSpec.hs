{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Cookie.JarSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Cookie.JarSpec (spec) where

import Test.Hspec
import Web.Cookie.Jar

spec :: Spec
spec = describe "Jar" $ do
  context "endsThen" $
    it "should be a simple test" $ do
      getIt `endsThen` (== (Just "a string"))


getIt :: IO (Maybe String)
getIt = pure $ Just "a string"
