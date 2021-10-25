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

import           Control.Monad (liftM4)

import           Debug.Trace(trace)

import qualified Data.Ord

-- Memory ADT
data GameMemory = GameMemory{
                  bid       :: Int,
                  actions   :: String,
                  rank      :: Int ,
                  winStreak :: Int,
                  count     :: Int,
                  currCount :: Int,
                  cardsSeen :: Int,
                  currCardS :: Int
                  } deriving Show

-- LookUp types for actions to be performed, a LookUpTable is an array of LookUpItems
type LookUpItem = (Int, [Rank], Points -> Action, String)
type LookUpTable = [LookUpItem]

-- LookUpTable contant for onlyStanding actions, will be explained further below under basicStrategy
onlyStandH :: Int
onlyStandH = 9

onlyStandS :: Int
onlyStandS = 3

-- List of functions used for GameMemory to String conversion
funcList :: [GameMemory -> String]
funcList = [show . bid, actions, show . rank, show . winStreak, show . count, show . currCount, show . cardsSeen, show . currCardS]

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard a b c d e f
    | trace ("\nplayerPoints: " ++ show b ++ "\nplayerId: " ++ show d ++"\nMyMemory:" ++ show e) False = undefined
    | otherwise = playingStrategy a b c d e f

playingStrategy :: PlayFunc
-- This very first round of bidding, we will use minBid to bid for the first round
playingStrategy Nothing _ _ _  Nothing _ = (Bid minBid, show minBid++"|B|0|0|0|0|0|0")

-- This is the case where we bid in other rounds 
playingStrategy Nothing playerPoints playerInfo playerID (Just memory) _ = biddingStrategy playerPoints playerID playerInfo memory

-- This is the case for when we perform actions other than bidding 
playingStrategy (Just dealerCard) playerPoints playerInfo playerID (Just memory) myHand = findStrat where
    myPoints = foldr (\pp acc -> if playerID == _playerPointsId pp then _playerPoints pp else acc) 0 playerPoints
    findStrat = basicStrategyHardTotal dealerCard myPoints playerInfo (stringToMemory memory) myHand

-- This case will not happen, an error will be thrown if it does!
playingStrategy _ _ _ _ _ _ = error "Error in playingStrategy."

-- | Converts whatever player points to a ranking system. The more points you have the more, the higher your ranking will be.
playerPointsToRank :: [PlayerPoints] -> PlayerId -> Int
playerPointsToRank playerPoints playerId =
    case elemIndex playerId $ _playerPointsId <$> sortOn (Data.Ord.Down . _playerPoints) playerPoints of
    Just player -> player + 1
    Nothing     -> error "Player not found."

-- | Aggregates the playerInfo into number of cards for a certain collection of playerInfo in a ROUND
playerInfoToLength :: [PlayerInfo]  -> Int
playerInfoToLength playerInfo = foldr ((+) . length) 0 (playerInfoHand <$> playerInfo)

-- | Helper function that converts a collection of playerInfo into number of cards
playerInfosToCount :: [PlayerInfo] -> Int
playerInfosToCount playerInfo = convertHandsToCount $ playerInfoHand <$> playerInfo

-- | Helper function that converts a card into a card value, which can be -1, 1 or 0. These cards are based off the Hi-Lo card counting strategy.
convertCardToCount :: Card -> Int
convertCardToCount card
    | getRank card `elem` Ace : [Ten .. King] = -1
    | getRank card `elem` [Two .. Six] = 1
    | getRank card `elem` [Seven .. Nine] = 0
    | otherwise = error "Error in convertCardToCount."

-- | Helper function that converts a particular hand into a total count for the hand based off the Hi-Lo card counting strategy. 
convertHandToCount :: Hand -> Int
convertHandToCount hand = sum $ convertCardToCount <$> hand

-- | Helper function that converts multiple hands into a total count for the hand based off the Hi-Lo card counting strategy. 
convertHandsToCount :: [Hand] -> Int
convertHandsToCount hands = sum $ convertHandToCount <$> hands

-- | A carefully designed and succinct function used to convert old GameMemory into a new memory string based off an array of incoming new data.
addToMemory :: GameMemory -> [(String, Int)] -> String
addToMemory oldMemory newData = intercalate "|" $ foldr (\(inf, pos) prev -> change pos inf prev) (($ oldMemory) <$> funcList) newData

-- | A function that changes the item at a position of a list by taking in a position, a new item and a list to change as input. 
change :: Int -> a -> [a] -> [a]
change position a lst = take position lst ++ a : drop (position + 1) lst

-- | Conversion of string to custom GameMemory data using the parser and getting its result. Here the intricacies of conversion is abstracted away when used in later functions 
stringToMemory :: String -> GameMemory
stringToMemory myMemory = getParsedResult $ parse parseMemory myMemory

-- PARSING EXTENSION FOR PARSING PLAYER ACTION -- 
-- Here, the lower case d represents to HIT after double down, capital D represents to STAND
parseAction :: Parser Char
parseAction = is 'B' ||| is 'S' ||| is 'H' ||| is 'D' ||| is 'd' ||| is 'I' ||| is 'S' ||| is 'L'

parseActions :: Parser [Char]
parseActions = list parseAction

-- PARSER EXTENSION FOR BID PARSER -- 
parseDigit :: Parser Char
parseDigit = is '-' ||| is '0' ||| is '1' ||| is '2' ||| is '3' ||| is '4' ||| is '5' ||| is '6' ||| is '7' ||| is '8' ||| is '9'

-- | parseNumber converts the string into an int at the very end using read and fmap
-- >>> parse parseNumber "100"
parseNumber :: Parser Int
parseNumber = read <$> list parseDigit

-- PARSER EXTENSION FOR MEMORY PARSER --
-- >>> parse parseMemory "100|BHHS|1|3"
-- Result >< GameMemory {bid = 100, actions = "BHHS", rank = 1, winStreak = 3}
--
-- >>> parse parseMemory "100|BHHS|9|5"
-- Result >< GameMemory {bid = 100, actions = "BHHS", rank = 9, winStreak = 5}
--
-- >>> isErrorResult (parse parseMemory "100|BHHS|a|p")
-- True
parseMemory :: Parser GameMemory
parseMemory = do
    bid          <- parseNumber
    _            <- is '|'
    actions      <- parseActions
    _            <- is '|'
    rank         <- parseNumber
    _            <- is '|'
    winStreak    <- parseNumber
    _            <- is '|'
    count        <- parseNumber
    _            <- is '|'
    seen         <- parseNumber
    _            <- is '|'
    curSeen      <- parseNumber
    _            <- is '|'
    GameMemory bid actions rank winStreak count seen curSeen <$> parseNumber

-- PARSER EXTENSION FOR OBTAINING PARSED RESULT -- 
getParsedResult :: ParseResult a -> a
getParsedResult (Result _ res) = res
getParsedResult (Error e) = error (show e)

-- UTILITY PARSERS -- 
list :: Parser a -> Parser [a]
list p1 = list1 p1 ||| pure []

list1 :: Parser a -> Parser [a]
list1 p = p >>= (\p' -> list p >>= (\p''-> pure (p':p'')))

-- BASIC STRATEGY IMPLEMENTATION --
basicStrategyHardTotal :: Card -> Points -> [PlayerInfo] -> GameMemory -> Hand ->  (Action, String)
basicStrategyHardTotal (Card _ rank) myPoints playerInfo gameMemory myHand = findAction where

    bids :: Int
    bids = bid gameMemory

    interMemory :: Int
    interMemory = cardsSeen gameMemory + playerInfoToLength playerInfo 

    interMemory' :: Int
    interMemory' = count gameMemory + playerInfosToCount playerInfo

    actionsDone :: String
    actionsDone = actions gameMemory

    newMemory :: [(String, Int)] -> String
    newMemory = addToMemory gameMemory

    tableUsed :: LookUpTable
    tableUsed = determineTable myHand myPoints bids

    finalBasic :: (Points -> Action, String)
    finalBasic = basicAction (lookupQ (handCalc myHand) tableUsed) rank

    findAction
        | last actionsDone == 'D' = (Hit, newMemory [("d", 1), (show interMemory', 5), (show interMemory, 7)])
        | last actionsDone == 'd' = (Stand, newMemory [("S", 1), (show interMemory', 5),(show interMemory, 7)])
        | otherwise = (fst finalBasic bids, newMemory [(snd finalBasic, 1), (show interMemory', 5),(show interMemory, 7)])

determineTable :: [Card] -> Points -> Points -> LookUpTable
determineTable myHand currPoints bidPoints
    | length myHand == 2 && currPoints >= (2 * bidPoints) = case myHand of
        [Card _ Ace, _] -> basicTableSoft
        [Card _ x, Card _ s] -> if x == s then basicTableSplit s else basicTableHard
        _ -> error "Tried to get action for cards of more than length 3 in determineTable."
    | length myHand == 2 && currPoints < (2 * bidPoints) = case myHand of
        [Card _ Ace, _] -> take onlyStandS basicTableSoft
        _ -> take onlyStandH basicTableHard
    | otherwise = take onlyStandH basicTableHard

basicAction :: Maybe ([Rank] , Points -> Action, String) -> Rank -> (Points -> Action, String)
basicAction Nothing _ = (const Hit, "H")
basicAction (Just (ranks, action, act)) oppHand =
    if oppHand `elem` ranks then (action, act) else (const Hit, act)

-- BIDDING STRATEGY IMPLEMENTATION -- 
biddingStrategy :: [PlayerPoints] -> PlayerId -> [PlayerInfo] -> String -> (Action, String)
biddingStrategy playerPoints playerId playerInfo oldMemory = finalBid where

    gameMem :: GameMemory
    gameMem = stringToMemory oldMemory

    interMemory'' :: Int
    interMemory'' = mod (currCardS gameMem + cardsSeen gameMem + playerInfoToLength playerInfo) 156

    interMemory' :: Int
    interMemory' = div 156 (1 + interMemory'')

    interMemory :: Int
    interMemory = div (currCount gameMem + playerInfosToCount playerInfo) interMemory'

    prevRank :: Int
    prevRank = rank gameMem

    currRank :: Int
    currRank = playerPointsToRank playerPoints playerId

    myPoints:_ = filter ((playerId == ). _playerPointsId) playerPoints

    currStreak :: Int
    currStreak = winStreak gameMem

    streaks :: Int -> Int -> Int
    streaks prevR currR = if currR - prevR > 0 then currStreak + 1 else 0

    finalAmount :: Int
    finalAmount = interMemory * minBid + (streaks prevRank currRank * 2)

    finalBid
        | finalAmount >= maxBid && _playerPoints myPoints >= maxBid =
            (Bid maxBid, addToMemory gameMem [(show maxBid, 0), (show (streaks prevRank currRank), 3), (show currRank, 2), (show interMemory, 4),(show interMemory'', 6)])

        | finalAmount <= minBid && _playerPoints myPoints >= minBid =
            (Bid minBid, addToMemory gameMem [(show minBid, 0), (show (streaks prevRank currRank), 3), (show currRank, 2), (show interMemory, 4), (show interMemory'', 6)])

        | _playerPoints myPoints <= minBid || _playerPoints myPoints < finalAmount = 
            (Bid (_playerPoints myPoints), addToMemory gameMem [(show (_playerPoints myPoints), 0), (show (streaks prevRank currRank), 3), (show currRank, 2), (show interMemory, 4), (show interMemory'', 6)])
        
        | otherwise = 
            (Bid finalAmount, addToMemory gameMem [(show finalAmount, 0), (show (streaks prevRank currRank), 3), (show currRank, 2), (show interMemory, 4), (show interMemory'', 6)])


basicTableHard :: LookUpTable
basicTableHard = [(21, [Ace .. King], const Stand, "S"),
                  (20, [Ace .. King], const Stand, "S"),
                  (19, [Ace .. King], const Stand, "S"),
                  (18, [Ace .. King], const Stand, "S"),
                  (17, [Ace .. King], const Stand, "S"),
                  (16, [Two ..  Six], const Stand, "S"),
                  (15, [Two ..  Six], const Stand, "S"),
                  (14, [Two ..  Six], const Stand, "S"),
                  (13, [Two ..  Six], const Stand, "S"),
                  (11, [Ace .. King],  DoubleDown, "D"),
                  (10, [Two .. Nine],  DoubleDown, "D"),
                  (9,  [Three .. Six], DoubleDown, "D")]

basicTableSoft :: LookUpTable
basicTableSoft = [(21, [Ace .. King], const Stand, "S"),
                  (20, [Ace .. King], const Stand, "S"),
                  (19, [Ace .. King], const Stand, "S"),
                  (18, [Two ..  Six],  DoubleDown, "D"),
                  (17, [Two ..  Six],  DoubleDown, "D"),
                  (16, [Four .. Six],  DoubleDown, "D"),
                  (15, [Four .. Six],  DoubleDown, "D"),
                  (14, [Five .. Six],  DoubleDown, "D"),
                  (13, [Five .. Six],  DoubleDown, "D")]

basicTableSplit :: Rank -> LookUpTable
basicTableSplit rank = case rank of
    Ace -> [(12, [Ace .. King],      Split, "L")]
    _   -> [(18, Nine : [Two .. Six], Split, "L"),
            (16, [Ace .. King],      Split, "L"),
            (14, [Two ..  Seven],    Split, "L"),
            (12, [Two .. Six],       Split, "L"),
            (8,  [Five .. Six],      Split, "L"),
            (6,  [Two .. Seven],     Split, "L"),
            (4,  [Two .. Seven],     Split, "L")]


-- UTILITY FUNCTIONS FOR LOOKUP TABLE --
lookupQ :: (Eq a) => a -> [(a,b,c,d)] -> Maybe (b, c, d)
lookupQ _key [] =  Nothing
lookupQ key ((x,y,z,a):xyzas)
    | key == x  =  Just (y, z, a)
    | otherwise =  lookupQ key xyzas
