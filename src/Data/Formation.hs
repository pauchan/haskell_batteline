module Data.Formation where

import Data.List

type Deck = [Card]
data GameState = GameState { deck :: Deck, player1 :: Player, player2 :: Player }
type Hand = [Card]
data Formation = Formation [Card] 
--type Formation = [Card]
data FormationType = Host | Skirmish | Battalion | Phalanx | Wedge
data Card = Card {color :: Color,
                  value :: Int
                  } deriving ( Eq )
data Player = Player { hand :: Hand, table :: [FlagStatus] } deriving ( Show )
data FlagStatus = FlagStatus { flag :: Flag, formation :: Formation }
data Color = Blue | Green | Orange | Purple | Red | Yellow deriving (Show, Enum, Eq)
--data Card = Card Color Int deriving (Show)
data Flag = One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Enum, Eq, Show)
data PlayerNumber = Player1 | Player2

instance Show Card where
  show card = (show $ color card) ++ " " ++ (show $ value card)

instance Show Formation where
  show formation = show $ cards formation

instance Show FlagStatus where
  show status = show $ formation status

showFlags :: [FlagStatus] -> [FlagStatus] -> String
showFlags [] [] = ""
showFlags [x] [y] = showFlag x y
showFlags (x:xs) (y:ys) = showFlag x y ++ showFlags xs ys

showFlag :: FlagStatus -> FlagStatus -> String
showFlag x y = show x ++ " | " ++ show y ++ "\n"

showStateForPlayer :: GameState -> Player -> Player -> String
showStateForPlayer state player opponent = (show $ hand player) ++ "\n" ++ (showFlags (table player) (table opponent))

cards :: Formation -> [Card]
cards (Formation cardlist) = cardlist

count :: Formation -> Int
count formation = length $ cards formation

formationType :: Formation -> Maybe FormationType
formationType formation
  | isWedge formation = Just Wedge
  | isPhalanx formation = Just Phalanx
  | isBattalion formation = Just Battalion
  | isSkirmish formation = Just Skirmish
  | isHost formation = Just Host
  | otherwise = Nothing

isWedge :: Formation -> Bool
isWedge formation = isSameColor formation && isConsequtiveValue formation

isPhalanx :: Formation -> Bool
isPhalanx formation = isSameValue formation

isBattalion :: Formation -> Bool
isBattalion formation = isSameColor formation

isSkirmish :: Formation -> Bool
isSkirmish formation = isConsequtiveValue formation

isSameValue :: Formation -> Bool
isSameValue formation = do
  let first = value $ head $ cards formation
  let values = map value $ cards formation
  let sameCount = filter (== first) values
  (==3) $ length sameCount

isSameColor :: Formation -> Bool
isSameColor formation = do
  let first = color $ head $ cards formation
  let colors = map color $ cards formation
  let sameCount = filter (== first) colors
  (==3) $ length sameCount

isConsequtiveValue :: Formation -> Bool
isConsequtiveValue formation = do
  let values = map value $ cards formation
  let sortedList = sort values
  length sortedList == 3 && sortedList !! 0 == (sortedList !! 1) - 1 && sortedList !! 0 == (sortedList !! 2) - 2

isHost :: Formation -> Bool
isHost formation = count formation == 3
