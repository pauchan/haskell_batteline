module Main where

import System.Random (newStdGen, randoms, StdGen)
import Data.List (sortOn)
import Data.Formation

--color :: Card -> Color
--color (Card col _) = col

--value :: Card -> Int
--value (Card _ val) = val

shuffle :: StdGen -> [a] -> [a]
shuffle gen = map snd . sortOn fst . zip (randoms gen :: [Int])

exit :: IO ()
exit = undefined

winning :: GameState -> Bool
winning = undefined

gameLoop :: GameState -> IO GameState
gameLoop gameState = turn Player1 gameState >>= turn Player2

turn :: PlayerNumber -> GameState -> IO GameState
turn playerNumber gameState = do
  let player = getPlayer playerNumber gameState
  print $  hand player
  draw playerNumber <$> playCard playerNumber gameState

playCard :: PlayerNumber -> GameState -> IO GameState
playCard player gameState = do
  card <- chooseCard player gameState
  flag <- chooseFlag player gameState
  return $ updateGame player card flag gameState

draw :: PlayerNumber -> GameState -> GameState
draw playerNumber gameState = updatePlayer gameState'
  where
    (topCard:deck') = deck gameState
    gameState' = gameState { deck = deck' }
    player = getPlayer playerNumber gameState
    player' =  player { hand = topCard:(hand player) }
    updatePlayer gameState = case playerNumber of 
                                  Player1 -> gameState { player1 = player'}
                                  Player2 -> gameState { player2 = player' }

getPlayer :: PlayerNumber -> GameState -> Player
getPlayer playerNumber gameState = case playerNumber of
                                        Player1 -> player1 gameState
                                        Player2 -> player2 gameState 

chooseFlag :: PlayerNumber -> GameState -> IO Flag
chooseFlag playerNumber gameState = go
    where
        go = getFlag >>= maybe go return
        getFlag = flagFromString <$> ask "Please pick a flag"

flagFromString :: String -> Maybe Flag
flagFromString "1" = Just One
flagFromString "2" = Just Two
flagFromString "3" = Just Three
flagFromString "4" = Just Four
flagFromString "5" = Just Five
flagFromString "6" = Just Six
flagFromString "7" = Just Seven
flagFromString "8" = Just Eight
flagFromString "9" = Just Nine
flagFromString _ = Nothing

--updateFlag :: [FlagStatus] -> Card -> [FlagStatus
--updateFlag flagStatus card = 

addCard :: Formation -> Card -> Formation
addCard formation card = Formation $ ((cards formation) ++ [card])

isSelectedFlag :: FlagStatus -> Flag -> Bool
isSelectedFlag flagStatus f = flag flagStatus == f

addCardToFlag :: FlagStatus -> Card -> FlagStatus
addCardToFlag flagStatus card = FlagStatus { formation = addCard (formation flagStatus) card }

updatePlayer :: Player -> Card -> Flag -> Player
updatePlayer player card flag = do
  let newFlagStatus = map (\x -> if isSelectedFlag x flag then addCardToFlag x card else x) $ table player
  Player { table = newFlagStatus }

updateGame :: PlayerNumber -> Card -> Flag -> GameState -> GameState
updateGame player card flag gameState = undefined

ask :: String -> IO String
ask message = putStrLn message >> getLine

chooseCard :: PlayerNumber -> GameState -> IO Card
chooseCard playerNumber gameState = do
    cardIndex <- ask "Pick a card please"
    return $ hand player !! read cardIndex
  where
    player = getPlayer playerNumber gameState

runGame :: GameState -> IO ()
runGame gameState = do
  gameState' <- gameLoop gameState
  if winning gameState' then
    exit
  else
    runGame gameState'

initialTable :: [FlagStatus]
initialTable = [FlagStatus One $ Formation [],
  FlagStatus Two $ Formation [],
  FlagStatus Three $ Formation [],
  FlagStatus Four $ Formation [],
  FlagStatus Five $ Formation [],
  FlagStatus Six $ Formation [],
  FlagStatus Seven $ Formation [],
  FlagStatus Eight $ Formation [],
  FlagStatus Nine $ Formation [] ]

initialDeck :: StdGen -> Deck
initialDeck gen = shuffle gen [Card color value | color <- [Blue .. Yellow], value <- [1..10]]

initialState :: StdGen -> GameState
initialState gen = do
  let deck = initialDeck gen
  let (player1hand, deck') = splitAt 7 deck
  let (player2hand, deck'') = splitAt 7 deck'
  GameState { deck = deck''
                             , player1 = Player player1hand initialTable
                             , player2 = Player player2hand initialTable
                             }

main :: IO ()
main = do
  gen <- newStdGen
  runGame $ initialState gen
