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
-- Get normal player done by successfully parsing "PLAYER_ACTION|PLAYER_BID"

-- Memory ADT
data GameMemory = GameMemory 
    {
        prevActions ::[Char], 
        prevBids    ::[Char],
        cardCount   ::[[CardCountValue]] 
    } 
    deriving Show

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard dealerCard playerPoints playerInfo playerID myMemory myHand
    | trace ("dCard: "++ show dealerCard++  " player id= "++ show playerID++ " player info: "++ show playerInfo++ " " ++ show playerID++"player points: "++ show playerPoints++ " myHand: "++ show myHand++ " myMemory: "++ show myMemory) False = undefined
    | otherwise = undefined



-- PARSER EXTENSION FOR PLAYERINFO --
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
parseAction :: Parser Char
parseAction = is 'H' ||| is 'S' ||| is 'B' ||| is 'd' ||| is 'D' ||| is 'I'

parseActions :: Parser String
parseActions = list parseAction

-- PARSER EXTENSION FOR BID PARSER -- 
parseDigit :: Parser Char
parseDigit = is '0' ||| is '1' ||| is '2' ||| is '3' ||| is '4' ||| is '5' ||| is '6' ||| is '7' ||| is '8' ||| is '9'

parseNumber :: Parser [Char]
parseNumber =  list parseDigit

-- PARSER EXTENSION FOR MEMORY PARSER --
parseMemory :: Parser GameMemory
parseMemory = parseActions >>= \act -> parseNumber >>= \bid -> parsePlayerInfos >>= \pInfo -> pure (GameMemory act bid pInfo)

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
sepby1 p1 sep = p1 >>= \i -> list (sep >> p1) >>= \i' -> pure(i:i')

-- UTILITY FUNCTIONS -- 
-- | Convert a list of int characters to digit 
listToInt :: [Char] -> Int
listToInt = read