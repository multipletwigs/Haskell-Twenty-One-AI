{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Parser.Instances
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game

import           Debug.Trace(trace)
import           GHC.Show (Show)

-- Source code changes:
-- Changed points please remember to change back to 10k

-- Todo:
-- Determine what to put in memory 
-- Memory has a format of "PLAYER_ACTION|PLAYER_BID|PLAYER_INFO|NUMBER_OF_CARDS_SEEN|RUNNING_COUNT"
-- RUNNING_COUNT is obtained from parsing PLAYER_INFO and converting it following Hi-Lo count 
-- After finish parsing, we will obtain a list of [[PLAYER_ACTION],PLAYER_BID,[PLAYER_INFO],NUMBER_OF_CARDS_SEEN,RUNNING_COUNT]

-- Todo URGENT:
-- Get normal player done by successfully parsing "PLAYER_ACTION|PLAYER_BID" along with basic strategy

-- Memory ADT
data GameMemory = GameMemory
    {
        bid         :: String,
        actions     :: String,
        cardsSeen   :: String,
        runningCount :: String,
        trueCount   :: String
    }
    deriving Show

biddingUnit :: Int
biddingUnit = 10

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard a b c d e f 
    | trace ("myMemory: " ++ show e) False = undefined
    | otherwise=  playingStrategy a b c d e f

playingStrategy :: PlayFunc
-- This very first round of bidding, we will use minBid to bid 
playingStrategy Nothing _ _ _  Nothing _ = (Bid maxBid, show maxBid++"|B|0|0|0")
playingStrategy Nothing playerPoints playerInfo playerID (Just memory) myHand = (Bid maxBid, memory)
playingStrategy (Just dealerCard) playerPoints playerInfo playerID (Just memory) myHand =
    basicStrategyHardTotal dealerCard myHand playerInfo playerID (stringToMemory memory)
playingStrategy _ _ _ _ _ _ = undefined

checkNewDeck :: GameMemory -> String
checkNewDeck = undefined

newRunningCountD :: [PlayerInfo] -> PlayerId -> Hand -> String -> Card -> String
newRunningCountD playerInfo playerId myHand oldMemory dealerCard =
    show $ read (runningCount (stringToMemory oldMemory)) + roundCardCountD playerInfo playerId myHand dealerCard

newRunningCount :: [PlayerInfo] -> PlayerId -> Hand -> String -> String
newRunningCount playerInfo playerId myHand oldMemory =
    show $ read (runningCount (stringToMemory oldMemory)) + roundCardCount playerInfo playerId myHand

memoryToCount :: GameMemory -> Int -> String
memoryToCount myMemory addedCount =
    show $ read (trueCount myMemory) + addedCount

roundCardCount :: [PlayerInfo] -> PlayerId -> Hand -> Int
roundCardCount playerInfo playerId myHand =
    convertHandsToCount (playerInfosToHands playerInfo playerId) + convertHandToCount myHand

roundCardCountD :: [PlayerInfo] -> PlayerId -> Hand -> Card -> Int
roundCardCountD playerInfo playerId myHand dealerCard =
    convertHandsToCount (playerInfosToHands playerInfo playerId) + convertHandToCount myHand + convertCardToCount dealerCard

stringToMemory :: String -> GameMemory
stringToMemory oldMemory =
    getParsedResult $ parse parseMemory oldMemory

memoryToString :: GameMemory -> String
memoryToString gameMemory =
    bid gameMemory ++ "|" ++ actions gameMemory ++ "|" ++ cardsSeen gameMemory ++ "|" ++ runningCount gameMemory ++ "|" ++ trueCount gameMemory

addActionToMemoryString :: GameMemory -> String -> String
addActionToMemoryString gameMemory action =
    bid gameMemory ++ "|" ++ actions gameMemory ++ action ++ "|" ++ cardsSeen gameMemory ++ "|" ++ runningCount gameMemory ++ "|" ++ trueCount gameMemory

addBidToMemoryString :: GameMemory -> Int -> String
addBidToMemoryString gameMemory bid =
    show bid ++ "|" ++ actions gameMemory ++ "B" ++ "|" ++ cardsSeen gameMemory ++ "|" ++ runningCount gameMemory ++ "|" ++ trueCount gameMemory

addRunningToMemoryString :: GameMemory -> String -> String
addRunningToMemoryString gameMemory count =
    bid gameMemory ++ "|" ++ actions gameMemory ++ "B" ++ "|" ++ cardsSeen gameMemory ++ "|" ++ count ++ "|" ++ trueCount gameMemory

-- |PlayerInfoToString filters out playerInfoHands that is about the player 
-- $setup
-- >>>    player1 = PlayerInfo {_playerInfoId = "1", playerInfoHand = [(Card Heart Ace), (Card Diamond Seven)]} 
-- >>>    player2 = PlayerInfo {_playerInfoId = "2", playerInfoHand = [(Card Heart Seven), (Card Diamond Ten)]} 
-- >>>    player3 = PlayerInfo {_playerInfoId = "3", playerInfoHand = [(Card Spade Ace), (Card Heart Six)]} 
-- >>>    playerInfo = [player1, player2, player3]
-- >>>    playerInfosToHands playerInfo "3"
-- [[HA,D7],[H7,DT]]
playerInfosToHands :: [PlayerInfo] -> PlayerId -> [Hand]
playerInfosToHands playerInfo playerId = playerInfoHand <$> filter ((playerId /= ). _playerInfoId) playerInfo

playerHandsToCount :: [Hand] -> Int
playerHandsToCount = convertHandsToCount

convertCardToCount :: Card -> Int
convertCardToCount card
    | getRank card `elem` [Ace, Ten, Queen, Jack, King] = -1
    | getRank card `elem` [Two, Three, Four, Five, Six] = 1
    | getRank card `elem` [Seven, Eight, Nine]          = 0
    | otherwise = 0

convertHandToCount :: Hand -> Int
convertHandToCount hand = sum $ convertCardToCount <$> hand

convertHandsToCount :: [Hand] -> Int
convertHandsToCount hands = sum $ convertHandToCount <$> hands


-- PARSING EXTENSION FOR PARSING PLAYERINFO TO COUNT -- 
data CardCountValue = High | Neutral | Low
parseSpade :: Parser Suit
parseSpade  = is 'S' >> pure Spade

parseHeart :: Parser Suit
parseHeart  = is 'H' >> pure Heart

parseClub :: Parser Suit
parseClub  = is 'C' >> pure Club

parseDiamond :: Parser Suit
parseDiamond  = is 'D' >> pure Diamond

parseSuit :: Parser Suit
parseSuit = parseSpade ||| parseHeart ||| parseClub ||| parseDiamond

parseHi:: Parser CardCountValue
parseHi = (is 'A' ||| is 'T' ||| is 'Q' ||| is 'K' ||| is 'J') >> pure High

parseNeutral :: Parser CardCountValue
parseNeutral = (is '7' ||| is '8' ||| is '9') >> pure Neutral

parseLow :: Parser CardCountValue
parseLow = (is '2' ||| is '3' ||| is '4' ||| is '5' ||| is '6') >> pure Low

parseRankToCount :: Parser CardCountValue
parseRankToCount = parseHi ||| parseNeutral ||| parseLow

parseCardToCount :: Parser CardCountValue
parseCardToCount = parseSuit >> parseRankToCount

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
parseMemory :: Parser GameMemory
-- >>> parse parseMemory "BHHS|100|[\"4\" [],\"3\" [],\"6\" [],\"0\" [],\"2\" [],\"9\" [],\"5\" [],\"7\" [],\"8\" [],\"1\" []]|40|10"
-- Result >< GameMemory {prevActions = "BHHS", prevBids = "100", playerInfo = [[],[],[],[],[],[],[],[],[],[]], cardsSeen = "40", trueCount = "10"}
--
parseMemory = do
    bid          <- parseNumber
    _            <- is '|'
    actions      <- parseActions
    _            <- is '|'
    cardsSeen    <- parseNumber
    _            <- is '|'
    runningCount <- parseNumber
    _            <- is '|'
    trueCount    <- parseNumber
    return $ GameMemory bid actions "0" "0" "0"

-- PARSER EXTENSION FOR OBTAINING PARSED RESULT -- 
getParsedResult :: ParseResult a -> a
getParsedResult (Result _ res) = res
getParsedResult (Error _) = error "Did not parse successfully"

-- UTILITY PARSERS -- 
list :: Parser a -> Parser [a]
list p1 = list1 p1 ||| pure []

list1 :: Parser a -> Parser [a]
list1 p = p >>= (\p' -> list p >>= (\p''-> pure (p':p'')))

sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 p1 sep = p1 >>= \i -> list (sep >> p1) >>= \i' -> pure(i:i')

-- BASIC STRATEGY IMPLEMENTATION -- 
basicStrategyHardTotal :: Card -> Hand -> [PlayerInfo] -> PlayerId -> GameMemory -> (Action, String)
basicStrategyHardTotal (Card _ rank) myHand playerInfo playerID gameMemory = findAction where
    total = handCalc myHand
    bids = read $ bid gameMemory
    actionsDone = actions gameMemory
    runningCount = newRunningCount playerInfo playerID myHand (memoryToString gameMemory)
    runningCountTest = addRunningToMemoryString gameMemory runningCount
    findAction
        | last actionsDone == 'D' = (Hit, addActionToMemoryString (stringToMemory runningCountTest) "d")
        | last actionsDone == 'd' = (Stand, addActionToMemoryString (stringToMemory runningCountTest) "S")
        | total == 17 && rank `elem` dealerSHDSet = (Stand, addActionToMemoryString (stringToMemory runningCountTest) "S")
        | total >= 13 && total <= 16 && rank `elem` dealerStandSet1 = (Stand, addActionToMemoryString (stringToMemory runningCountTest) "S")
        | total >= 13 && total <= 16 && rank `elem` dealerHitSet1 = (Hit, addActionToMemoryString (stringToMemory runningCountTest) "H")
        | total == 12 && (rank `elem` dealerHitSet3) || (rank `elem` dealerHitSet1) = (Hit, addActionToMemoryString (stringToMemory runningCountTest) "H")
        | total == 12 && rank `elem` dealerStandSet2 = (Stand, addActionToMemoryString (stringToMemory runningCountTest) "S")
        | total == 12 && rank `elem` dealerHitSet2 = (Hit, addActionToMemoryString (stringToMemory runningCountTest) "H")
        | total == 11 && rank `elem` dealerSHDSet && (length myHand == 2) = (DoubleDown bids, addActionToMemoryString (stringToMemory runningCountTest) "D")
        | total == 10 && rank `elem` dealerDoubleSet1 && (length myHand == 2) = (DoubleDown bids, addActionToMemoryString (stringToMemory runningCount) "D")
        | total == 10 && rank `elem` dealerHitSet2 && (length myHand == 2) = (DoubleDown bids, addActionToMemoryString (stringToMemory runningCountTest) "D")
        | total == 9 && rank `elem` dealerHitSet2 && (length myHand == 2) = (DoubleDown bids, addActionToMemoryString (stringToMemory runningCountTest) "D")
        | total == 9 && (rank `elem` dealerHitSet4) || (rank `elem` dealerHitSet1) = (Hit, addActionToMemoryString (stringToMemory runningCountTest) "H")
        | total == 8 = (Hit, addActionToMemoryString (stringToMemory runningCountTest) "H")
        | otherwise = (Hit, addActionToMemoryString (stringToMemory runningCountTest) "H")



samePair :: Hand -> Bool
samePair [a,b] = getRank a == getRank b
samePair _ = undefined

dealerSHDSet :: [Rank]
dealerSHDSet =
    [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Ace]

dealerStandSet1:: [Rank]
dealerStandSet1 = [Two, Three, Four, Five, Six]

dealerStandSet2:: [Rank]
dealerStandSet2 = [Four, Five, Six]

dealerHitSet1 :: [Rank]
dealerHitSet1 = [Seven, Eight, Nine, Ten, Ace]

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