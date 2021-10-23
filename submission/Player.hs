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
data GameMemory = GameMemory{ bid :: Int, actions :: String, rank :: Int , winStreak :: Int} deriving Show
type LookUpTable = [(Int, [Rank], Points -> Action, String)]

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard a b c d e f
    | trace ("PlayerId: " ++ show d ++ "\nMemory: " ++ show e ++ "\nHand: " ++ show f) False = undefined
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
playerPointsToRank :: [PlayerPoints] -> PlayerId -> Int
playerPointsToRank playerPoints playerId = case elemIndex playerId $ _playerPointsId <$> sortOn _playerPoints playerPoints of
    Just player -> player + 1
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
     intercalate "|" (change position newData (($ oldMemory) <$> [show . bid, actions, show . rank, show . winStreak]))

change :: Int -> a -> [a] -> [a]
change position a lst =
    take position lst ++ a : drop (position + 1) lst

stringToMemory :: String -> GameMemory
stringToMemory myMemory =
    getParsedResult $ parse parseMemory myMemory

-- PARSING EXTENSION FOR PARSING PLAYER ACTION -- 
-- Here, the lower case d represents to HIT after double down, capital D represents to STAND
parseAction :: Parser Char
parseAction = is 'B' ||| is 'S' ||| is 'H' ||| is 'D' ||| is 'd' ||| is 'I' ||| is 'S' ||| is 'L'

parseActions :: Parser [Char]
parseActions = list parseAction

-- PARSER EXTENSION FOR BID PARSER -- 
parseDigit :: Parser Char
parseDigit = is '-' ||| is '0' ||| is '1' ||| is '2' ||| is '3' ||| is '4' ||| is '5' ||| is '6' ||| is '7' ||| is '8' ||| is '9'

-- >>> parse parseNumber "100"
parseNumber :: Parser Int
parseNumber = read <$> list parseDigit

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

    total :: Int
    total = handCalc myHand

    bids :: Int
    bids = bid gameMemory

    actionsDone :: String
    actionsDone = actions gameMemory

    newMemory :: String -> Int -> String 
    newMemory = addToMemory gameMemory

    tableUsed :: LookUpTable
    tableUsed = findTable myHand

    finalBasic :: (Points -> Action, String)
    finalBasic = basicAction (lookupQ total tableUsed) rank

    finalHard' :: (Points -> Action, String)
    finalHard' = basicAction (lookupQ total (take 5 tableUsed)) rank

    findAction
        | last actionsDone == 'D' = (Hit, newMemory "d" 1)
        | last actionsDone == 'd' = (Stand, newMemory "S" 1)
        | length myHand == 2 = (fst finalBasic bids, newMemory (snd finalBasic) 1)
        | otherwise = (fst finalHard' bids, newMemory (snd finalHard') 1)

findTable :: [Card] -> LookUpTable
findTable myHand = case myHand of
    [Card _ Ace, _] -> basicTableSoft
    [Card _ x, Card _ s] -> if x == s then basicTableSplit s else basicTableHard 
    _ -> basicTableHard

basicAction :: Maybe ([Rank] , Points -> Action, String) -> Rank -> (Points -> Action, String)
basicAction Nothing _ = (const Hit, "H")
basicAction (Just (ranks, action, act)) oppHand =
    if oppHand `elem` ranks then (action, act) else (const Hit, act)

basicTableHard :: LookUpTable
basicTableHard = [(17, [Ace .. King], const Stand, "S"),
                  (16, [Two ..  Six], const Stand, "S"),
                  (15, [Two ..  Six], const Stand, "S"),
                  (14, [Two ..  Six], const Stand, "S"),
                  (13, [Two ..  Six], const Stand, "S"),
                  (11, [Ace .. King],  DoubleDown, "D"),
                  (10, [Two .. Nine],  DoubleDown, "D"),
                  (9,  [Three .. Six], DoubleDown, "D")]

basicTableSoft :: LookUpTable
basicTableSoft = [(20, [Ace .. King], const Stand, "S"),
                  (19, [Ace .. King], const Stand, "S"),
                  (18, [Two ..  Six],  DoubleDown, "D"),
                  (17, [Two ..  Six],  DoubleDown, "D"),
                  (16, [Four ..Six],   DoubleDown, "D"),
                  (15, [Four .. Six],  DoubleDown, "D"),
                  (14, [Five .. Six],  DoubleDown, "D"),
                  (13, [Five .. Six],  DoubleDown, "D")]

basicTableSplit :: Rank -> LookUpTable
basicTableSplit rank = case rank of 
    Ace -> [(12, [Ace .. King],      Split, "L")]
    _   -> [(18, [Nine, Two .. Six], Split, "L"),
           (16, [Ace .. King],      Split, "L"),
           (14, [Two ..  Seven],    Split, "L"),
           (12, [Two .. Six],      Split, "L"),
           (8,  [Five .. Six],      Split, "L"),
           (6,  [Two .. Seven],     Split, "L"),
           (4,  [Two .. Seven],     Split, "L")]
    

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
    prevRank = rank gameMem

    currRank :: Int
    currRank = playerPointsToRank playerPoints playerId

    currStreak :: Int
    currStreak = winStreak gameMem

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

