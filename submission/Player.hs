-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Parser.Instances
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game

import           Debug.Trace(trace)
import GHC.Show (Show)

-- Source code changes:
-- Changed points please remember to change back to 10k

-- Todo:
-- Determine what to put in memory 
-- Memory has a format of "PLAYER_ACTION|PLAYER_BID|PLAYER_INFO|NUMBER_OF_CARDS_SEEN|DECKS_REMAINING|RUNNING_COUNT"
-- RUNNING_COUNT is obtained from parsing PLAYER_INFO and converting it following Hi-Lo count 
-- After finish parsing, we will obtain a list of [[PLAYER_ACTION],PLAYER_BID,[PLAYER_INFO],NUMBER_OF_CARDS_SEEN,DECKS_REMAINING,RUNNING_COUNT]

-- Todo URGENT:
-- Get normal player done by successfully parsing "PLAYER_ACTION|PLAYER_BID"

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard dealerCard playerPoints playerInfo playerID myMemory myHand
    | trace ("dCard: "++ show dealerCard++  " player id= "++ show playerID++ " player info: "++ show playerInfo++ " " ++ show playerID++"player points: "++ show playerPoints++ " myHand: "++ show myHand++ " myMemory: "++ show myMemory) False = undefined
    | otherwise = playStrat dealerCard playerID myMemory myHand

playStrat :: Maybe Card -> PlayerId -> Maybe String -> Hand -> (Action, String)
-- playStrat Nothing _ Just myMemory _ = findBestBid myMemory parsePlayerInfos
playStrat _ _ (Just myMemory) _ = (Hit, myMemory)
playStrat _ _ _ _ = (Stand, "")

-- ACTION FUNCTIONS--

-- | Always hit no matter the input, and returns action with same memory
alwaysHit :: String -> (Action, String)
alwaysHit = (,) Hit

-- | Always stand no matter the input, and returns action with same memory
alwaysStand :: String -> (Action, String)
alwaysStand = (,) Stand

-- ALGORITHM FUNCTIONS --
getValueCount :: CardCountValue -> Int 
getValueCount High = 1
getValueCount Neutral = 0
getValueCount Low = -1

getHandCount :: [CardCountValue] -> Int 
getHandCount hand = sum $ getValueCount <$> hand 

findBestBid :: String -> Parser [[CardCountValue]] -> (Action, String)
findBestBid myMemory parser =  (Bid toBid, myMemory ++ "B" ++ show toBid) where 
    result = getParsedResult $ parse parser myMemory
    handCount = getHandCount <$> result
    totalCardCount = sum handCount 
    toBid = (totalCardCount - 1) * biddingUnit

-- naiveBid :: String -> (Action, String)
-- naiveBid myMemory = (20, myMemory ++ "B")

-- PARSER EXTENSIONS FOR PARSING PLAYER INFO-- 

biddingUnit :: Int 
biddingUnit = 20

data CardCountValue = High | Neutral | Low
instance Show CardCountValue where
    show High = "High"
    show Low = "Low"
    show Neutral = "Neutral"
-- >>> parse parseSpade "S"
-- Result >< S
parseSpade :: Parser Suit 
parseSpade  = is 'S' >> pure Spade

parseHeart :: Parser Suit 
parseHeart  = is 'H' >> pure Heart

parseClub :: Parser Suit 
parseClub  = is 'C' >> pure Club

parseDiamond :: Parser Suit 
parseDiamond  = is 'D' >> pure Diamond

-- | Parses the suit of the card
-- >>> parse parseSuit "HT"
-- Result >T< H
--
-- >>> parse parseSuit "S2"
-- Result >2< S
--
-- >>> parse parseSuit "D9"
-- Result >9< D

parseSuit :: Parser Suit
parseSuit = parseSpade ||| parseHeart ||| parseClub ||| parseDiamond

-- | Parses the Rank of a card and gives a CardCountValue
-- >>> parse parseHi "A"
-- Result >< High
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

parseID :: Parser Char
parseID = is '"' >> character >> is '"' >> is ' '

parseHand :: Parser [CardCountValue]
parseHand = is '[' >> sepby1 parseCardToCount (is ',') <* is ']'

parsePlayerInfo :: Parser [CardCountValue]
parsePlayerInfo = parseID >>= const parseHand >>= pure 

parsePlayerInfos :: Parser [[CardCountValue]]
parsePlayerInfos = is '[' >> sepby1 parsePlayerInfo (is ',') <* is ']'

-- PARSING EXTENSION FOR PARSING PLAYER ACTION -- 
data PlayerAction = PBid | PDoubleDown1 | PDoubleDown2 | PHit | PStand | PInsurance | PSplit
instance Show PlayerAction where
    show PBid = "B"
    show PDoubleDown1 = "d"
    show PDoubleDown2 = "D"
    show PHit= "H"
    show PStand = "S"
    show PInsurance = "B"
    show PSplit = "P"

parseBid :: Parser PlayerAction
parseBid  = is 'B' >> pure PBid

parseDoubleDown1 :: Parser PlayerAction
parseDoubleDown1 = is 'd' >> pure PDoubleDown1

parseDoubleDown2 :: Parser PlayerAction
parseDoubleDown2 = is 'D' >> pure PDoubleDown2

parseHit :: Parser PlayerAction
parseHit = is 'H' >> pure PHit

parseStand :: Parser PlayerAction
parseStand = is 'S' >> pure PStand

parseInsurance :: Parser PlayerAction
parseInsurance = is 'I' >> pure PInsurance

parseSplit :: Parser PlayerAction
parseSplit = is 'P' >> pure PSplit

parseAction :: Parser PlayerAction
parseAction = parseBid ||| parseDoubleDown1 ||| parseDoubleDown2 ||| parseStand ||| parseInsurance ||| parseHit ||| parseSplit

parseActions :: Parser [PlayerAction]
parseActions = list parseAction

-- PARSER EXTENSION FOR MEMORY PARSER --
data Memory = Memory {actions::[PlayerAction], infos :: [[CardCountValue]]} deriving Show
finalParse :: Parser Memory
finalParse = parseActions >>= \i -> parsePlayerInfos >>= \i' -> pure (Memory i i')

getMemory :: ParseResult a -> a
getMemory (Result _ a)  = a
getMemory (Error _) = error "test"

-- PARSER EXTENSION FOR OBTAINING PARSED RESULT (FILLER) -- 
getParsedResult :: ParseResult a -> a
getParsedResult (Result _ res) = res
getParsedResult (Error _) = error "Did not parse successfully"

-- UTILITY PARSERS -- 
list :: Parser a -> Parser [a]
list p1 = list1 p1 ||| pure []

list1 :: Parser a -> Parser [a]
list1 p = p >>= (\p' -> list p >>= (\p''-> pure (p':p'')))

sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 p1 sep = p1 >>= \i -> (list (sep >> p1) >>= \i' -> pure(i:i'))

