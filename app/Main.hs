module Main where

import System.Random (newStdGen, randoms, StdGen)
import Data.List (sortOn)
import Data.Tuple (swap)
import Data.Formation

shuffle :: StdGen -> [a] -> [a]
shuffle gen = map snd . sortOn fst . zip (randoms gen :: [Int])

exit :: IO ()
exit = undefined

showFlags :: [FlagStatus] -> [FlagStatus] -> GameState -> String
showFlags [] [] state = ""
showFlags [x] [y] state = showFlag x y state
showFlags (x:xs) (y:ys) state = showFlag x y state ++ showFlags xs ys state

showFlag :: FlagStatus -> FlagStatus -> GameState -> String
showFlag x y state = show x ++ " | " ++ show y ++ (takenString x y state) ++ "\n"

showStateForPlayer :: GameState -> Player -> Player -> String
showStateForPlayer state player opponent = (show $ hand player) ++ "\n" ++ (showFlags (table player) (table opponent) state)

winning :: GameState -> PlayerNumber -> Bool
winning state playerNumber = do
  let zs = zipStates state playerNumber
  let opponent = getPlayer (getOpponet playerNumber) state
  let pc = privateCards (deck state) (hand opponent)
  let winningFlags = map (winningFlag pc) zs
  fiveWinning winningFlags || threeConsqutive 0 winningFlags

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

zipStates :: GameState -> PlayerNumber -> [(Formation, Formation)]
zipStates state playerNumber = zip (getFormation state playerNumber) (getFormation state $ getOpponet playerNumber)

getFormation :: GameState -> PlayerNumber -> [Formation]
getFormation state Player1 = map formation $ table (player1 state)
getFormation state Player2 = map formation $ table (player2 state)

fiveWinning :: [Bool] -> Bool
fiveWinning winnings = (foldl (+) 0 $ map boolToInt winnings) >= 5

threeConsqutive :: Int -> [Bool] -> Bool
threeConsqutive count [] = count >= 3
threeConsqutive count [x] = threeConsqutive (checkConsequtive count x) []
threeConsqutive count (x:xs) = threeConsqutive (checkConsequtive count x) xs

checkConsequtive :: Int -> Bool -> Int
checkConsequtive count True = count + 1
checkConsequtive count False = 0

winningFlag :: [Card] -> (Formation, Formation) -> Bool
winningFlag privateCards (myFlag,opponentFlag) = do
  let myFlagStatus = formationType myFlag
  let opponentFlagStatus = formationType opponentFlag
  case (myFlagStatus, opponentFlagStatus) of 
    (Nothing, _) -> False
    (Just x, Just y) -> x > y
    (Just x, Nothing) -> noPossibleWinners x opponentFlag privateCards

noPossibleWinners :: FormationType -> Formation -> [Card] -> Bool
noPossibleWinners t formation [] = True
noPossibleWinners t formation [x] = case formationType (addCard formation x) of
  Nothing -> True
  Just form -> t > form
noPossibleWinners t formation (x:xs) = case formationType (addCard formation x) of
  Nothing -> noPossibleWinners t formation xs
  Just x -> t > x

privateCards :: [Card] -> [Card] -> [Card]
privateCards deck opponentHand = deck ++ opponentHand

getOpponet :: PlayerNumber -> PlayerNumber
getOpponet Player1 = Player2
getOpponet Player2 = Player1

turn :: PlayerNumber -> GameState -> IO GameState
turn playerNumber gameState = do
  let player = getPlayer playerNumber gameState
  let opponent = getPlayer (getOpponet playerNumber) gameState
  putStr $ showStateForPlayer gameState player opponent
  draw playerNumber <$> playCard playerNumber gameState

playCard :: PlayerNumber -> GameState -> IO GameState
playCard player gameState = do
  card <- chooseCard player gameState
  flag <- chooseFlag player gameState
  if playable flag player gameState then
    return $ updateGame player card flag gameState
  else  
    playCard player gameState

playable :: Flag -> PlayerNumber -> GameState -> Bool
playable flag playerNumber state = do
  let flagState = (table $ getPlayer playerNumber state) !! (intFromFlag flag)
  let maxThreeCards = length (cards (formation flagState)) >= 3
  not (maxThreeCards || takenBy flag Player1 state || takenBy flag Player2 state)

takenBy :: Flag -> PlayerNumber -> GameState -> Bool
takenBy flag playerNumber state = do
  let pc = privateCards (deck state) (hand $ getPlayer playerNumber state)
  let flagState = zipStates state playerNumber !! (intFromFlag flag)
  winningFlag pc flagState

takenString :: FlagStatus -> FlagStatus -> GameState -> String
takenString flagStatus oppStatus state = do
  let p1 = takenBy (flag flagStatus) Player1 state
  let p2 = takenBy (flag oppStatus) Player2 state
  case (p1, p2) of 
    (True, _) -> " taken by player 1"
    (False, True) -> " taken by player 2"
    otherwise -> " flag unoccupied"

draw :: PlayerNumber -> GameState -> GameState
draw playerNumber gameState = updatePlayer gameState'
  where
    (topCard:deck') = deck gameState
    gameState' = gameState { deck = deck' }
    player = getPlayer playerNumber gameState
    player' =  player { hand = topCard:(hand player) }
    updatePlayer gameState = case playerNumber of 
                                  Player1 -> setPlayer1 player' gameState
                                  Player2 -> setPlayer2 player' gameState

getPlayer :: PlayerNumber -> GameState -> Player
getPlayer playerNumber gameState = case playerNumber of
                                        Player1 -> player1 gameState
                                        Player2 -> player2 gameState 


setPlayer1 :: Player -> GameState -> GameState
setPlayer1 player state = GameState { deck = deck state, player1 = player, player2 = player2 state }

setPlayer2 :: Player -> GameState -> GameState
setPlayer2 player state = GameState { deck = deck state, player1 = player1 state , player2 = player }

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

intFromFlag :: Flag -> Int
intFromFlag One = 0
intFromFlag Two = 1
intFromFlag Three = 2
intFromFlag Four = 3
intFromFlag Five = 4
intFromFlag Six = 5
intFromFlag Seven = 6
intFromFlag Eight = 7
intFromFlag Nine = 8

addCard :: Formation -> Card -> Formation
addCard formation card = Formation $ ((cards formation) ++ [card])

isSelectedFlag :: Flag -> FlagStatus -> Bool
isSelectedFlag f flagStatus = flag flagStatus == f

--getFlagStatus :: GameState -> Flag -> FlagStatus
--getFlagStatus state flag = head (filter (isSelectedFlag flag) state  

addCardToFlag :: FlagStatus -> Card -> Flag -> FlagStatus
addCardToFlag flagStatus card flag = FlagStatus { flag = flag, formation = addCard (formation flagStatus) card }

updatePlayer :: Player -> Card -> Flag -> Player
updatePlayer player card flag = do
  let newFlagStatus = map (\x -> if isSelectedFlag flag x then addCardToFlag x card flag else x) $ table player
  let newHand = updateHand (hand player) card
  Player { hand = newHand, table = newFlagStatus }

updateHand :: Hand -> Card -> Hand
updateHand hand card = filter (/= card) hand

updateGame :: PlayerNumber -> Card -> Flag -> GameState -> GameState
updateGame player card flag gameState = do
  let p = getPlayer player gameState
  case player of
    Player1 -> setPlayer1 (updatePlayer p card flag) gameState
    Player2 -> setPlayer2 (updatePlayer p card flag) gameState


ask :: String -> IO String
ask message = putStrLn message >> getLine

chooseCard :: PlayerNumber -> GameState -> IO Card
chooseCard playerNumber gameState = do
    cardIndex <- ask "Pick a card please"
    return $ hand player !! read cardIndex
  where
    player = getPlayer playerNumber gameState

gameLoop :: GameState -> IO GameState
gameLoop gameState = turn Player1 gameState >>= turn Player2

runGame :: GameState -> IO ()
runGame gameState = do
  gameState' <- turn Player1 gameState
  if winning gameState' Player1 then
    exit
  else do
    gameState'' <- turn Player2 gameState'
    if winning gameState'' Player2 then
      exit
    else
      runGame gameState''

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
