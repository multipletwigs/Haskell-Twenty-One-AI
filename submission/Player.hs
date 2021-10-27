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
import           TwentyOne.Play (dealerId)

-- Memory ADT --
-- *THINGS TO NOTE* 
-- 1. rank refers to the ranking of the player in the last round, the higher the rank the more points you have in [PlayerPoints].
-- 2. winStreak increases when there is a positive change in point ranking, based off on last round and a current round.  
-- 3. runCount refers to the runningCount kept track throughout the game.
-- 4. currCount tracks the count of ONE particular turn for a player. 
-- 5. cardsSeen refers to the true number of cards seen within the shoe, kept track using `mod` 156. 
-- 6. currCardsS refers to the number of cards seen for ONE particular turn for a player. 

data GameMemory = GameMemory
                  {
                             bid       :: Int, actions   :: String, -- Ensures that game rules are followed, by keeping track of bid and actions.
                             rank      :: Int, winStreak :: Int,    -- A statistic of winning streak as defined by a positive rank increase.
                             runCount  :: Int, currCount :: Int,    -- A statistic of keeping the true count and a round count. 
                             cardsSeen :: Int, currCardS :: Int     -- A statistic of keeping the total cards seen and a round cards seen count. 
                  }          deriving Show

-- LookUp types for actions to be performed, a LookUpTable is an array of LookUpItems
type LookUpItem = (Int, [Rank], Points -> Action, String)
type LookUpTable = [LookUpItem]

-- | LookUpTable contant for onlyStanding actions, will be explained further below under basicStrategy
onlyStandH :: Int -- Selecting onlyStand section of hard totals
onlyStandH = 9

onlyStandS :: Int -- Selecting onlyStand section of soft totals
onlyStandS = 3

-- List of functions used for GameMemory to String conversion
funcList :: [GameMemory -> String]
funcList = [show . bid, actions, show . rank, show . winStreak, show . runCount, show . currCount, show . cardsSeen, show . currCardS]

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard = playingStrategy -- This is left here so that I could trace the code using debug.trace without changing too much of code. 

playingStrategy :: PlayFunc
-- This very first round of bidding, we will use minBid to bid for the first round
playingStrategy Nothing _ _ _  Nothing _ = (Bid minBid, show minBid++"|B|0|0|0|0|0|0")

-- This is the case where we bid in other rounds 
playingStrategy Nothing playerPoints playerInfo playerID (Just memory) _ = biddingStrategy playerPoints playerID playerInfo (stringToMemory memory)

-- This is the case for when we perform actions other than bidding 
playingStrategy (Just dealerCard) playerPoints playerInfo playerID (Just memory) myHand = findStrat where
    myPoints = foldr (\pp acc -> if playerID == _playerPointsId pp then _playerPoints pp else acc) 0 playerPoints
    findStrat = basicStrategy dealerCard myPoints playerInfo (stringToMemory memory) myHand playerID

-- This case will not happen, an error will be thrown if it does! Here just to silence any unmatched pattern warnings. 
playingStrategy _ _ _ _ _ _ = error "Error in playingStrategy."

-- PARSING EXTENSION FOR PARSING PLAYER ACTION -- 
-- Here, the lower case d represents to HIT after double down, capital D represents to STAND
parseAction :: Parser Char
parseAction = is 'B' ||| is 'S' ||| is 'H' ||| is 'D' ||| is 'd' ||| is 'I' ||| is 'L'

parseActions :: Parser [Char]
parseActions = list parseAction

-- PARSER EXTENSION FOR BID PARSER -- 
-- | parseDigit parses a single digit which can then be converted to an int
parseDigit :: Parser Char
parseDigit = is '-' ||| is '0' ||| is '1' ||| is '2' ||| is '3' ||| is '4' ||| is '5' ||| is '6' ||| is '7' ||| is '8' ||| is '9'

-- | parseNumber converts the string into an int at the very end using read and fmap for parsers
parseNumber :: Parser Int
parseNumber = read <$> list parseDigit

-- PARSER EXTENSION FOR MEMORY PARSER --
-- parseMemory is a parser combinator that parses the following sections of the string. 
parseMemory :: Parser GameMemory
parseMemory = do
    bid          <- parseNumber
    is '|'
    actions      <- parseActions
    is '|'
    rank         <- parseNumber
    is '|'
    winStreak    <- parseNumber
    is '|'
    runCount     <- parseNumber
    is '|'
    currCount    <- parseNumber
    is '|'
    seen         <- parseNumber
    is '|'
    GameMemory bid actions rank winStreak runCount currCount seen <$> parseNumber

-- PARSER EXTENSION FOR OBTAINING PARSED RESULT -- 
-- Source: Course Notes by Tim Dwyer
getParsedResult :: ParseResult a -> a
getParsedResult (Result _ res) = res
getParsedResult (Error e) = error (show e)

-- UTILITY PARSERS -- 
-- Source: Tutorial 11 MA_GROUP 6 GROUP ANSWERS
list :: Parser a -> Parser [a]
list p1 = list1 p1 ||| pure []

list1 :: Parser a -> Parser [a]
list1 p = p >>= (\p' -> list p >>= (\p''-> pure (p':p'')))

-- BASIC STRATEGY IMPLEMENTATION --
basicStrategy :: Card -> Points -> [PlayerInfo] -> GameMemory -> Hand -> PlayerId -> (Action, String)
basicStrategy (Card _ rank) myPoints playerInfo oldMemory myHand playerId = findAction where

    -- Obtains the bid of a previous round
    prevBid :: Int
    prevBid = bid oldMemory

    -- Sums up TOTAL cards seen from previous round and current cards seen in playerInfo
    roundCardsSeen :: Int
    roundCardsSeen = cardsSeen oldMemory + playerInfosToLength (removeDealer playerInfo) (Just playerId) 

    -- Sums up RUNNING count from previous round and current count seen in playerInfo
    roundCount :: Int
    roundCount = runCount oldMemory + playerInfosToCount (removeDealer playerInfo) (Just playerId)

    -- Obtains the last action done by the player
    actionsDone :: String
    actionsDone = actions oldMemory

    -- Partial function that allows addition to memory using addToMemory function
    newMemory :: [(String, Int)] -> String
    newMemory = addToMemory oldMemory

    -- Determine what lookup table to use out of the three lookup tables
    tableUsed :: LookUpTable
    tableUsed = determineTable myHand myPoints prevBid

    -- Determines the FINAL action to perform based on the lookup table search
    finalBasic :: (Points -> Action, String)
    finalBasic = basicAction (lookupQ (handCalc myHand) tableUsed) rank

    -- Determines the next action to do based on lookUpTable as well as passing on NEW information to the next playFunc call
    findAction
        | last actionsDone == 'D' = (Hit, newMemory [("d", 1), (show roundCount, 5), (show roundCardsSeen, 7)])
        | last actionsDone == 'd' = (Stand, newMemory [("S", 1), (show roundCount, 5),(show roundCardsSeen, 7)])
        | otherwise = (fst finalBasic prevBid, newMemory [(snd finalBasic, 1), (show roundCount, 5),(show roundCardsSeen, 7)])

-- | determineTable helps us find which table that we are supposed to use from the 3 different lookUp Tables. 
-- Understand that there are many conditions to keep track of to ensure the next action is valid. 
-- For instance, if your hand has more than 2 cards, meaning the first sub-round is over, you cannot DoubleDown or Split anymore. 
determineTable :: [Card] -> Points -> Points -> LookUpTable
determineTable myHand currPoints bidPoints
    | length myHand == 2 && currPoints >= (2 * bidPoints) = case myHand of -- Ensures that we have enough points to DoubleDown and Split
        [Card _ Ace, _] -> basicTableSoft
        [Card _ x, Card _ s] -> if x == s then basicTableSplit s else basicTableHard
        _ -> error "Tried to get action for cards of more than length 3 in determineTable."
    | length myHand == 2 && currPoints < (2 * bidPoints) = case myHand of  -- Ensures we DO NOT split or doubledown when not enough points
        [Card _ Ace, _] -> take onlyStandS basicTableSoft
        _ -> take onlyStandH basicTableHard    
    | otherwise = take onlyStandH basicTableHard -- Ensures that only Hit and Stand actions are performable after first sub-round

-- | basicAction determines what actions to take after a lookup table is found
-- After a lookup table is found, we have to ensure that the dealerUp card and your hand fulfills certain conditions before carrying out a move.
-- The lookup tables only encode information about DoubleDown, Stand and Splits. Hits are automatically handled if the lookUpQ function cannot find
-- an encoded action.
-- Source: https://www.blackjackapprenticeship.com/wp-content/uploads/2018/08/BJA_Basic_Strategy.jpg
basicAction :: Maybe ([Rank] , Points -> Action, String) -> Rank -> (Points -> Action, String)
basicAction Nothing _ = (const Hit, "H")
basicAction (Just (ranks, action, act)) oppHand =
    if oppHand `elem` ranks then (action, act) else (const Hit, act)

-- BIDDING STRATEGY IMPLEMENTATION -- 
biddingStrategy :: [PlayerPoints] -> PlayerId -> [PlayerInfo] -> GameMemory -> (Action, String)
biddingStrategy playerPoints playerId playerInfo oldMemory = finalBid where

    -- THE MATH FOR CARD COUNTING AND CARDSSEEN CALCULATION WILL BE DOCUMENTED AT THE BOTTOM OF THE CODE
    totalCardsSeen :: Int
    totalCardsSeen = currCardS oldMemory + cardsSeen oldMemory + playerInfosToLength playerInfo Nothing

    totalCardsSeen' :: Int
    totalCardsSeen' = mod totalCardsSeen (52 * numDecks)

    decksLeft :: Int
    decksLeft = numDecks - safeDiv totalCardsSeen 52

    runningCount :: Int 
    runningCount = 
        if totalCardsSeen >= (52 * numDecks) then 0 else currCount oldMemory + playerInfosToCount playerInfo Nothing

    trueCount :: Int
    trueCount = safeDiv runningCount decksLeft

    -- THE MATH FOR RANKING AND WIN STREAK CALCULATION WILL BE DOCUMENTED AT THE BOTTOM OF THE CODE
    currRank :: Int
    currRank = playerPointsToRank playerPoints playerId

    myPoints :: Points
    myPoints = findPoints playerPoints playerId

    totalStreak :: Int
    totalStreak = findStreaks oldMemory playerPoints playerId

    -- The final bidding amount is trueCount * minimumBid + winStreak * 2
    finalAmount :: Int
    finalAmount = trueCount * minBid + totalStreak * 2

    -- Array of new memory needed to be encoded 
    newMemory :: Int -> [(String, Int)]
    newMemory newBid = [(show newBid, 0), ("B", 1),(show currRank, 2),(show totalStreak, 3), (show runningCount, 4),(show totalCardsSeen', 6)]

    -- Determines the final bids, checking if our supposed bidding amount is following the rules of maxBid, minBid and all in, otherwise bid the supposed amount. 
    finalBid
        | finalAmount >= maxBid && myPoints >= maxBid = (Bid maxBid, addToMemory oldMemory $ newMemory maxBid)      -- Bid maximum case
        | finalAmount <= minBid && myPoints >= minBid = (Bid minBid, addToMemory oldMemory $ newMemory minBid)      -- Bid minimum case
        | myPoints <= minBid || myPoints < finalAmount = (Bid myPoints, addToMemory oldMemory $ newMemory myPoints) -- All In case
        | otherwise = (Bid finalAmount, addToMemory oldMemory $ newMemory finalAmount)  -- Normal bid case

-- A safe division when dealing with divide by 0. It will be further explained on why division by 0 is 1 at the bottom of the code
safeDiv :: Int -> Int -> Int
safeDiv _ 0 = 1
safeDiv num denom = num `div` denom

-- | findStreaks determines if there is a streak to increase. If there is a positive change in ranking for a round,
--   then the streak will increase by 1.     
findStreaks :: GameMemory -> [PlayerPoints] -> PlayerId -> Int
findStreaks oldMemory playerPoints playerId = streaks prevR currR where
    prevR :: Int            -- Ranking in the previous round
    prevR = rank oldMemory

    currR :: Int            -- Ranking in the current round
    currR = playerPointsToRank playerPoints playerId

    currStreak :: Int       -- Current winning streak
    currStreak = winStreak oldMemory

    streaks :: Int -> Int -> Int -- Increase current winning streak by 1 if there is a positive change in rank 
    streaks prevRank currRank = if currRank - prevRank > 0 then currStreak + 1 else 0


-- LOOKUP TABLES FOR BASIC STRATEGY -- 
-- Source: https://www.blackjackapprenticeship.com/wp-content/uploads/2018/08/BJA_Basic_Strategy.jpg
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
-- A modified lookUp function based on the lookUp function from Hoogle where we lookUp for list of tuples with 4 items in it instead
-- Referrence: https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:lookup
lookupQ :: (Eq a) => a -> [(a,b,c,d)] -> Maybe (b, c, d) 
lookupQ _key [] =  Nothing
lookupQ key ((x,y,z,a):xyzas)
    | key == x  =  Just (y, z, a)
    | otherwise =  lookupQ key xyzas

-- UTILITY FUNCTIONS TO FIND GAMEMEMORY METRICS --

-- | removeDealer is a function that removes dealerInformation from playerInfo so that it does not interfere with our calculations 
removeDealer :: [PlayerInfo] -> [PlayerInfo] -- This is used to remove dealer information from [PlayerInfo]
removeDealer = filter ((dealerId /= ) . _playerInfoId)

-- | Converts whatever player points to a ranking system. The more points you have the more, the higher your ranking will be.
playerPointsToRank :: [PlayerPoints] -> PlayerId -> Int
playerPointsToRank playerPoints playerId =
    case elemIndex playerId $ _playerPointsId <$> sortOn _playerPoints playerPoints of
    Just player -> player + 1
    Nothing     -> error "Player not found."

-- | Aggregates the playerInfo into number of cards for a certain collection of playerInfo in a ROUND
playerInfosToLength :: [PlayerInfo]  -> Maybe PlayerId -> Int
playerInfosToLength playerInfo (Just playerId) = foldr (\pp acc -> if playerId /= _playerInfoId pp then acc + length (playerInfoHand pp) else acc) 0 playerInfo
playerInfosToLength playerInfo Nothing = foldr ((+) . length) 0 (playerInfoHand <$> playerInfo)

-- | Aggregates a collection of playerInfo into card Hi-Lo count depending if playerId should be included in the count or not  
playerInfosToCount :: [PlayerInfo] -> Maybe PlayerId -> Int
playerInfosToCount playerInfo (Just playerId) = foldr (\pp acc -> if playerId /= _playerInfoId pp then acc + convertHandToCount (playerInfoHand pp) else acc) 0 playerInfo
playerInfosToCount playerInfo Nothing = foldr ((+) . convertHandToCount . playerInfoHand) 0 playerInfo

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

-- | Helper function that finds a players current points
findPoints :: [PlayerPoints] -> PlayerId -> Points
findPoints playerPoints playerId = _playerPoints myPoints where
    myPoints:_ = filter ((playerId == ) . _playerPointsId) playerPoints


-- UTILITY FUNCTIONS FOR MEMORY --
-- | A carefully designed and succinct function used to convert old GameMemory into a new memory string based off an array of incoming new data.
addToMemory :: GameMemory -> [(String, Int)] -> String
addToMemory oldMemory newData = intercalate "|" $ foldr (\(inf, pos) prev -> change pos inf prev) (sequenceA funcList oldMemory) newData

-- | A function that changes the item at a position of a list by taking in a position, a new item and a list to change as input. 
change :: Int -> a -> [a] -> [a]
change position a lst = take position lst ++ a : drop (position + 1) lst

-- | Conversion of string to custom GameMemory data using the parser and getting its result. Here the intricacies of conversion is abstracted away when used in later functions 
stringToMemory :: String -> GameMemory
stringToMemory myMemory = getParsedResult $ parse parseMemory myMemory

-- !Note: Math behind card counting
-- In order to obtain the running count of a card game, we need to recognize the fact that the playing round has player information complement to the player information to the bidding round. 
-- VERY FIRST PLAYING ROUND
-- EXAMPLE FOR PLAYER 1, in this example, player 1 is the fourth player: 
-- Playing Round -> ["7" [],"9" [],"4" [],"2" [],"0" [],"8" [],"1" [],"6" [DJ,H5,DK],"3" [DT,DA,C7,C2],"5" [C8,SJ,S6,S3]]
-- Memory -> "10|B|0|0|0|0|0|0", here count and cards seen is still 0 from very first bidding round
-- Metrics: 
-- currCount = -1, in this playing round, the round count is -1 
-- currCardS = 11, in this playing round there are 11 cards, excluding dealer hand and excluding player hand
-- Here we exclude the dealer hand and player hand because information is NOT the latest for these two at any given round. 
-- *PLAYER 1 STANDS, THE ROUND ENDS 

-- Bidding round 
-- ["6" [],"3" [],"5" [],"dealer" [HA,D9],"7" [HJ,S2,D6,H8],"9" [D8,HT],"4" [CQ,C9,C6],"2" [DK,S3,DQ],"0" [H8,HJ,S2],"8" [H7,HQ],"1" [DA,CT]]
-- Memory -> Just "10|S|0|0|0|-1|0|11", we can see that the currCount and currCardS is bought from last round to bidding round. 
-- In every bidding round, we have the complete information of the dealer, and our own cards + complete hands of player after us 
-- Metrics: 
-- cardsSeen = cards seen from last round (11) + cards seen in this round (21) = 32 
-- runningCount = currCount from last round (-1) + count from this round (-5) = -6 

-- In the bidding round, we can determine the trueCount of the running count by determining how many decks we have left. 
-- First we need to find out how many decks we have left based on the number of cards we have seen 
-- We can do this by having numDecks (3) - (cardsSeen / 52), since one deck has 52 cards, and maximum number of cards we can see is 52 * 3 = 156 cards at any given round. 

-- Once we have the number of decks left, we can then divide the runningCount with the number of decks left, by performing runningCount / number of decks left. 

-- The reason why I have created a safeDiv function is because there can be a case where cardsSeen is exactly 156, although rare, but it can happen. If the exact cardsSeen is 156, this means that the number of decks left calculated will be 0. And hence when dividing by the runningCount a division by 0 error can happen. So if there exist a case where there are EXACTLY 0 decks left, we will treat it as having one deck left.

-- If the cardsSeen amount exceed by 156, which is possible due to the way it is counted, then we know the whole shoe of cards is finished, and we reset the runningCount back to 0.