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
import Debug.Trace
-- Memory ADT
data GameMemory = GameMemory{ bid :: String, actions :: String, rank :: String , winStreak :: String} deriving Show

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard a b c d e f 
    | trace ("PlayerId: " ++ show d ++ "\nMemory: " ++ show e) False = undefined
    | otherwise = playingStrategy a b c d e f

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

playerInfoToLength :: [PlayerInfo] -> PlayerId -> Int
playerInfoToLength playerInfo playerId = 
    sum $ length <$> (playerInfoHand <$> filter ((playerId /= ). _playerInfoId) playerInfo)    

playerInfosToCount :: [PlayerInfo] -> PlayerId -> Int
playerInfosToCount playerInfo playerId = 
    convertHandsToCount $ playerInfoHand <$> filter ((playerId /= ). _playerInfoId) playerInfo

convertCardToCount :: Card -> Int
convertCardToCount card
    | getRank card `elem` [Ace, Ten .. King] = -1
    | getRank card `elem` [Two .. Six] = 1
    | getRank card `elem` [Seven .. Nine] = 0
    | otherwise = error "Error in convertCardToCount."

convertHandToCount :: Hand -> Int
convertHandToCount hand = sum $ convertCardToCount <$> hand

convertHandsToCount :: [Hand] -> Int
convertHandsToCount hands = sum $ convertHandToCount <$> hands

addToMemory :: GameMemory -> String -> Int -> String
addToMemory oldMemory newData position = 
    intercalate "|" (change position newData (($ oldMemory) <$> [bid, actions, rank, winStreak]))

change :: Int -> a -> [a] -> [a]
change position a lst = 
    take position lst ++ a : drop (position + 1) lst

stringToMemory :: String -> GameMemory
stringToMemory myMemory =
    getParsedResult $ parse parseMemory myMemory

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
    newMemory = addToMemory gameMemory

    final1 :: (Points -> Action, String)
    final1 = basicAction (lookupQ total basicTableHard) rank

    final2 :: (Points -> Action, String)
    final2 = basicAction (lookupQ total (take 5 basicTableHard)) rank

    findAction 
        | last actionsDone == 'D' = (Hit, newMemory "d" 1)
        | last actionsDone == 'd' = (Stand, newMemory "S" 1)
        | length myHand == 2 = (fst final1 bids, newMemory (snd final1) 1)
        | otherwise = (fst final2 bids, newMemory (snd final2) 1)  

basicAction :: Maybe ([Rank] , Points -> Action, String) -> Rank -> (Points -> Action, String) 
basicAction Nothing _ = (const Hit, "H") 
basicAction (Just (ranks, action, act)) oppHand = 
    if oppHand `elem` ranks then (action, act) else (const Hit, act)

basicTableHard :: [(Int, [Rank], Points -> Action, String)]
basicTableHard = [(17, [Ace .. King], const Stand, "S"),
                  (16, [Two ..  Six], const Stand, "S"),
                  (15, [Two ..  Six], const Stand, "S"),
                  (14, [Two ..  Six], const Stand, "S"),
                  (13, [Two ..  Six], const Stand, "S"),
                  (11, [Ace .. King], DoubleDown, "D"),
                  (10, [Two .. Nine], DoubleDown, "D"),
                  (9,  [Three .. Six], DoubleDown, "D")]

basicTableSoft :: [(Int, [Rank], Points -> Action, String)]
basicTableSoft = [(9, [Ace .. King], const Stand, "S"),
                  (8, [Ace .. King], const Stand, "S"),
                  (7, [Two ..  Six], DoubleDown, "D"),
                  (6, [Two ..  Six], DoubleDown, "D"),
                  (5, [Four ..Six],  DoubleDown, "D"),
                  (4, [Four .. Six], DoubleDown, "D"),
                  (3, [Five .. Six], DoubleDown, "D"),
                  (2, [Five .. Six], DoubleDown, "D")]

basicTableSplit :: [(Rank, [Rank], Points -> Action, String)]
basicTableSplit = [(Ace, [Ace .. King], Split, "Sp"),
                  (Nine, [Nine, Two .. Six], Split, "Sp"),
                  (Eight, [Ace .. King], Split, "Sp"),
                  (Seven, [Two ..  Seven], Split, "Sp"),
                  (Six, [Two ..  Six], Split, "Sp"),
                  (Four, [Five .. Six], Split, "Sp"),
                  (Three, [Two .. Six], Split, "Sp"),
                  (Two, [Two .. Seven], Split, "Sp")]

lookupQ :: (Eq a) => a -> [(a,b,c,d)] -> Maybe (b, c, d)
lookupQ _key [] =  Nothing
lookupQ key ((x,y,z,a):xyzas)
    | key == x  =  Just (y, z, a)
    | otherwise =  lookupQ key xyzas

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
    gameMemStreak = stringToMemory $ addToMemory gameMem (show (streaks prevRank currRank)) 3

    gameMemRank :: GameMemory
    gameMemRank = stringToMemory $ addToMemory gameMemStreak (show currRank) 2

    finalAmount :: Int 
    finalAmount = currRank * minBid + (streaks prevRank currRank * 10)

    finalBid 
        | finalAmount >= maxBid = (Bid maxBid, addToMemory gameMemRank (show maxBid) 0)
        | otherwise = (Bid finalAmount, addToMemory gameMemRank (show finalAmount) 0)
