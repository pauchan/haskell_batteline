module Main where

type Deck = [Card]
data GameState = GameState { deck :: Deck, player1 :: Player, player2 :: Player }
type Hand = [Card]
data Player = Player { hand :: Hand }
data Card = Card deriving (Show)
data Flag = One | Two | Thee | Four | Five | Six | Seven | Eight | Nine deriving Enum
data PlayerNumber = Player1 | Player2

exit :: IO ()
exit = undefined

winning :: GameState -> Bool
winning = undefined

gameLoop :: GameState -> IO GameState
gameLoop gameState = turn Player1 gameState >>= turn Player2

turn :: PlayerNumber -> GameState -> IO GameState
turn player gameState = draw player <$> playCard player gameState

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
chooseFlag playerNumber gameState = toEnum <$> (read <$> ask "please pick a flag")
    
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

initialState :: GameState
initialState = GameState { deck = []
                         , player1 = Player []
                         , player2 = Player []
                         }

main :: IO ()
main = runGame initialState
