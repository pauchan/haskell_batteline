module Data.Formation where

import Data.List

type Deck = [Card]
data GameState = GameState { deck :: Deck, player1 :: Player, player2 :: Player }
type Hand = [Card]
data Formation = Formation [Card] deriving ( Eq ) 
data FormationValue = FormationValue FormationType Int deriving ( Eq )
data FormationType = Host | Skirmish | Battalion | Phalanx | Wedge deriving ( Eq, Ord)
data Card = Card {color :: Color,
                  value :: Int
                  } deriving ( Eq )
data Player = Player { hand :: Hand, table :: [FlagStatus] } deriving ( Show )
data FlagStatus = FlagStatus { flag :: Flag, formation :: Formation }
data Color = Blue | Green | Orange | Purple | Red | Yellow deriving (Show, Enum, Eq)
data Flag = One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Enum, Eq, Show)
data PlayerNumber = Player1 | Player2

ftype :: FormationValue -> FormationType
ftype (FormationValue x _)  = x

fvalue :: FormationValue -> Int
fvalue (FormationValue _ y)  = y

instance Ord Formation where
  lhs `compare` rhs = do
    let lf = formationType lhs
    let rf = formationType rhs
    case (lf, rf) of
      (Nothing, Nothing) -> EQ
      (Just x, Nothing) -> GT
      (Nothing, Just y) -> LT
      (Just x, Just y) -> compare x y

instance Ord FormationValue where
  lhs `compare` rhs = if ftype lhs == ftype rhs then compare (fvalue lhs) (fvalue rhs) else compare (ftype lhs) (ftype rhs)

formValue :: Formation -> Int
formValue formation = foldl (+) 0 (map value (cards formation)) 


instance Show Card where
  show card = (show $ color card) ++ " " ++ (show $ value card)

instance Show Formation where
  show formation = show $ cards formation

instance Show FlagStatus where
  show status = show $ formation status

instance Show PlayerNumber where
  show Player1 = "Player1"
  show Player2 = "Player2"

cards :: Formation -> [Card]
cards (Formation cardlist) = cardlist

count :: Formation -> Int
count formation = length $ cards formation

formationType :: Formation -> Maybe FormationValue
formationType formation
  | isWedge formation = Just (FormationValue Wedge $ formValue formation)
  | isPhalanx formation = Just (FormationValue Phalanx $ formValue formation)
  | isBattalion formation = Just (FormationValue Battalion $ formValue formation)
  | isSkirmish formation = Just (FormationValue Skirmish $ formValue formation)
  | isHost formation = Just (FormationValue Host $ formValue formation)
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
