module Data.FormationSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Formation

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "formation" $ do
    it "Wedge is three cards with the same color and value" $ do
      isWedge (Formation (Card Red 3, Card Red 4, Card Red 5)) == True
