module Data.Formation where

--data Formation = Formation {formationType :: FormationType,
--                            formationValue :: Int
--                           } 
type Formation = [Card]

data FormationType = Host | Skirmish | Battalion | Phalanx | Wedge
data Card = Card {color :: Color,
                  value :: Int
                  }

data Color = Red | Blue | Purpule | Pink | Green | Yellow deriving (Show, Eq)

isWedge :: Formation -> Bool
isWedge formation = True
