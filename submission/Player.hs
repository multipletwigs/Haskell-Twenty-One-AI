{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Parser.Instances
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game

import           GHC.Show (Show)
import           Data.List
-- Memory ADT
data GameMemory = GameMemory{ bid :: String, actions :: String, rank :: String , winStreak :: String} deriving Show

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard = playingStrategy

playingStrategy :: PlayFunc
-- This very first round of bidding, we will use minBid to bid for the first round
playingStrategy Nothing _ _ _  Nothing _ = (Bid minBid, show minBid++"|B|0|0")

-- This is the case where we bid in other rounds 
playingStrategy Nothing playerPoints _ playerID (Just memory) _ = biddingStrategy playerPoints playerID memory

-- This is the case for when we perform actions other than bidding 
playingStrategy (Just dealerCard) _ _ _ (Just memory) myHand = basicStrategyHardTotal dealerCard myHand (stringToMemory memory)

-- This case will not happen, an error will be thrown if it does
playingStrategy _ _ _ _ _ _ = error "Error in playingStrategy"

-- The player will bid based on ranking, then the ranking information will be passed down into memory -- 
playerPointsToRank :: [PlayerPoints] -> PlayerId -> String
playerPointsToRank playerPoints playerId = case elemIndex playerId $ _playerPointsId <$> sortOn _playerPoints playerPoints of 
    Just player -> show (player + 1)
    Nothing     -> error " Player not found"

addRankToMemory :: GameMemory -> Int -> String 
addRankToMemory oldMemory newRank = 
    bid oldMemory ++ "|" ++ actions oldMemory ++ "|" ++ show newRank ++ "|" ++ winStreak oldMemory

addBidToMemory :: GameMemory -> String -> String 
addBidToMemory oldMemory bid = 
    bid ++ "|" ++ actions oldMemory ++ "B|" ++ rank oldMemory ++ "|" ++ winStreak oldMemory

addActionToMemory :: GameMemory -> String -> String 
addActionToMemory oldMemory act = 
    bid oldMemory ++ "|" ++ actions oldMemory ++ act ++ "|" ++ rank oldMemory ++ "|" ++ winStreak oldMemory

addStreakToMemory :: GameMemory -> Int -> String 
addStreakToMemory oldMemory streak = 
    bid oldMemory ++ "|" ++ actions oldMemory ++ "|" ++ rank oldMemory ++ "|" ++ show streak

stringToMemory :: String -> GameMemory
stringToMemory myMemory =
    getParsedResult $ parse parseMemory myMemory

-- This will convert our GameMemory to our String type
memoryToString :: GameMemory -> String
memoryToString gameMemory =
    bid gameMemory ++ "|" ++ actions gameMemory ++ "|" ++ rank gameMemory

-- PARSING EXTENSION FOR PARSING PLAYER ACTION -- 
-- Here, the lower case d represents to HIT after double down, capital D represents to STAND
parseAction :: Parser Char
parseAction = is 'B' ||| is 'S' ||| is 'H' ||| is 'D' ||| is 'd' ||| is 'I' ||| is 'S'

parseActions :: Parser [Char]
parseActions = list parseAction

-- PARSER EXTENSION FOR BID PARSER -- 
parseDigit :: Parser Char
parseDigit = is '-' ||| is '0' ||| is '1' ||| is '2' ||| is '3' ||| is '4' ||| is '5' ||| is '6' ||| is '7' ||| is '8' ||| is '9'

parseNumber :: Parser [Char]
parseNumber =  list parseDigit

-- PARSER EXTENSION FOR MEMORY PARSER --
-- >>> parse parseMemory "100|BHHS|1"
-- Result >< GameMemory {bid = "100", actions = "BHHS", rank = "1"}
--
-- >>> parse parseMemory "100|BHHS|1"
-- Result >< GameMemory {bid = "100", actions = "BHHS", rank = "9"}
parseMemory :: Parser GameMemory
parseMemory = do
    bid          <- parseNumber
    _            <- is '|'
    actions      <- parseActions
    _            <- is '|'
    rank         <- parseNumber
    _            <- is '|'
    GameMemory bid actions rank <$> parseNumber

-- PARSER EXTENSION FOR OBTAINING PARSED RESULT -- 
getParsedResult :: ParseResult a -> a
getParsedResult (Result _ res) = res
getParsedResult (Error e) = error (show e)

-- UTILITY PARSERS -- 
list :: Parser a -> Parser [a]
list p1 = list1 p1 ||| pure []

list1 :: Parser a -> Parser [a]
list1 p = p >>= (\p' -> list p >>= (\p''-> pure (p':p'')))

sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 p1 sep = p1 >>= \i -> list (sep >> p1) >>= \i' -> pure(i:i')

-- BASIC STRATEGY IMPLEMENTATION --
-- This section is meant to be messy, as it is a direct implementation of perfect basic strategy, please do ignore this --
basicStrategyHardTotal :: Card -> Hand -> GameMemory -> (Action, String)
basicStrategyHardTotal (Card _ rank) myHand gameMemory = findAction where
    total = handCalc myHand
    bids = read $ bid gameMemory
    actionsDone = actions gameMemory
    newMemory = addActionToMemory gameMemory
    findAction
        | last actionsDone == 'D' = (Hit, newMemory "d")
        | last actionsDone == 'd' = (Stand, newMemory "S")
        | total > 17 = (Stand, newMemory "S")
        | total == 17 && rank `elem` dealerSHDSet = (Stand, newMemory "S")
        | total >= 13 && total <= 16 && rank `elem` dealerStandSet1 = (Stand, newMemory "S")
        | total >= 13 && total <= 16 && rank `elem` dealerHitSet1 = (Hit, newMemory "H")
        | total == 12 && (rank `elem` dealerHitSet3) || (rank `elem` dealerHitSet1) = (Hit, newMemory "H")
        | total == 12 && rank `elem` dealerStandSet2 = (Stand, newMemory "S")
        | total == 12 && rank `elem` dealerHitSet2 = (Hit, newMemory "H")
        | total == 11 && rank `elem` dealerSHDSet && (length myHand == 2) = (DoubleDown bids, newMemory "D")
        | total == 10 && rank `elem` dealerDoubleSet1 && (length myHand == 2) = (DoubleDown bids, newMemory "D")
        | total == 10 && rank `elem` dealerHitSet2 && (length myHand == 2) = (DoubleDown bids, newMemory "D")
        | total == 9 && rank `elem` dealerHitSet2 && (length myHand == 2) = (DoubleDown bids, newMemory "D")
        | total == 9 && (rank `elem` dealerHitSet4) || (rank `elem` dealerHitSet1) = (Hit, newMemory "H")
        | total == 8 = (Hit, newMemory "H")
        | otherwise = (Hit, newMemory "H")

samePair :: Hand -> Bool
samePair [a,b] = getRank a == getRank b
samePair _ = undefined

dealerSHDSet :: [Rank]
dealerSHDSet = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Ace, Jack, Queen, King]

dealerStandSet1:: [Rank]
dealerStandSet1 = [Two, Three, Four, Five, Six]

dealerStandSet2:: [Rank]
dealerStandSet2 = [Four, Five, Six]

dealerHitSet1 :: [Rank]
dealerHitSet1 = [Seven, Eight, Nine, Ten, Ace, Jack, Queen, King]

dealerHitSet2 :: [Rank]
dealerHitSet2 = [Ten, Ace]

dealerHitSet3 :: [Rank]
dealerHitSet3 = [Two, Three]

dealerHitSet4 :: [Rank]
dealerHitSet4 = [Two]

dealerDoubleSet1 :: [Rank]
dealerDoubleSet1 = [Two, Three, Four, Five, Six, Seven, Eight, Nine]

dealerDoubleSet2 :: [Rank]
dealerDoubleSet2 = [Three, Four, Five, Six]


-- BIDDING STRATEGY -- 
biddingStrategy :: [PlayerPoints] -> PlayerId -> String -> (Action, String)
biddingStrategy playerPoints playerId oldMemory = finalBid where 

    gameMem :: GameMemory
    gameMem = stringToMemory oldMemory

    prevRank :: Int
    prevRank = read (rank gameMem)

    currRank :: Int
    currRank = read $ playerPointsToRank playerPoints playerId 

    currStreak :: Int 
    currStreak = read $ winStreak gameMem

    streaks :: Int -> Int -> Int 
    streaks prevR currR = if currR - prevR > 0 then currStreak + 1 else 0 

    gameMemStreak :: GameMemory
    gameMemStreak = stringToMemory $ addStreakToMemory gameMem (streaks prevRank currRank)

    gameMemRank :: GameMemory
    gameMemRank = stringToMemory $ addRankToMemory gameMemStreak currRank

    finalAmount :: Int 
    finalAmount = currRank * minBid + (streaks prevRank currRank * 2)

    finalBid 
        | finalAmount >= maxBid = (Bid maxBid, addBidToMemory gameMemRank (show maxBid))
        | otherwise = (Bid finalAmount, addBidToMemory gameMemRank (show finalAmount))
