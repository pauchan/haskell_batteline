module Main where

import System.Random (newStdGen, randoms, StdGen)
import Data.List (sortOn)

type Deck = [Card]
data GameState = GameState { deck :: Deck, player1 :: Player, player2 :: Player }
type Hand = [Card]
data Player = Player { hand :: Hand }
data Color = Blue | Green | Orange | Purple | Red | Yellow deriving (Show, Enum)
data Card = Card Color Int deriving (Show)
data Flag = One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving Enum
data PlayerNumber = Player1 | Player2

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
  show (hand player)
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
        getFlag = flagFromString <$> ask "Please pick aflag"

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

updateGame :: PlayerNumber -> Card -> Flag -> GameState -> GameState
updateGame = undefined

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

initialDeck :: StdGen -> Deck
initialDeck gen = shuffle gen [Card color value | color <- [Blue .. Yellow], value <- [1..10]]

initialState :: StdGen -> GameState
initialState gen = GameState { deck = initialDeck gen
                             , player1 = Player []
                             , player2 = Player []
                             }

main :: IO ()
main = do
  gen <- newStdGen
  runGame $ initialState gen
