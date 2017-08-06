module Data.FormationSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Formation

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "formation" $ do
    it "Wedge is three cards with the same color and consequtive values" $ do
      isWedge (Formation [Card Red 3, Card Red 4, Card Red 5]) == True
    it "Phalanx is three cards with the same value" $ do
      isPhalanx (Formation [Card Red 3, Card Blue 3, Card Green 3]) == True
    it "Battalion is three cards with the same color" $ do
      isBattalion (Formation [Card Red 3, Card Red 4, Card Red 6]) == True
    it "Skirmish is three consequtive cards" $ do
      isSkirmish (Formation [Card Red 3, Card Blue 4, Card Red 5]) == True
    it "Host is three cards with the same color and value" $ do
      isHost (Formation [Card Blue 3, Card Red 1, Card Red 5]) == True
    it "Three cards should have the same value" $ do
      isSameValue (Formation [Card Red 3, Card Blue 3, Card Green 3]) == True
    it "Three cards should not have the same value" $ do
      isSameValue (Formation [Card Red 3, Card Blue 4, Card Green 3]) == False
    it "Two cards should not have the same value" $ do
      isSameValue (Formation [Card Red 3, Card Blue 3]) == False
    it "Three cards should have the same color" $ do
      isSameColor (Formation [Card Red 3, Card Red 5, Card Red 7]) == True
    it "Three cards should not have the same color" $ do
      isSameColor (Formation [Card Red 3, Card Blue 4, Card Green 3]) == False
    it "Two cards should not have the same color" $ do
      isSameColor (Formation [Card Red 3, Card Red 4]) == False
    it "Three cards should have consequtive values" $ do
      isConsequtiveValue (Formation [Card Red 5, Card Red 3, Card Blue 4]) == True
    it "Three cards should not have consequtive values" $ do
      isConsequtiveValue (Formation [Card Red 3, Card Blue 4, Card Green 3]) == False
    it "Two cards should not have consequtive values" $ do
      isConsequtiveValue (Formation [Card Red 3, Card Red 4]) == False



