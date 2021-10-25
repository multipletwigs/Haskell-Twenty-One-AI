-- | TwentyOne-specific rules
module TwentyOne.Rules where

import           Deck
import           Cards
import           TwentyOne.Types
import           EitherIO
import           Data.Maybe
import           Data.List
import           Control.Monad
import           Debug.Trace
import           Utils

-- | Number of decks used in the game
numDecks :: Int
numDecks = 3

targetValue :: Int
targetValue = 21

startingNumCards :: Int
startingNumCards = 2

-- | Maximum number of cards in hand
maxCards :: Int
maxCards = 5

startingPoints :: Int
startingPoints = 10000

maxBid :: Int
maxBid = 100

minBid :: Int
minBid = 10

maxInsure :: Points -> Points
maxInsure = flip div 2

-- $setup
-- Define some variables for testing
-- >>> let pp1 = PlayerPoints "1"
--
-- Cards
-- >>> let h1 = Card Heart Ace
-- >>> let h2 = Card Heart Two
-- >>> let h3 = Card Heart Three
-- >>> let s1 = Card Spade Ace
-- >>> let s2 = Card Spade Two
-- >>> let s3 = Card Spade Three
-- >>> let cj = Card Club Jack
-- >>> let cq = Card Club Queen
-- >>> let ck = Card Club King
-- >>> let d1 = Card Diamond Ace
-- >>> let d5 = Card Diamond Five
-- >>> let d8 = Card Diamond Eight
--
-- Player
-- >>> let p1 = Player "1" undefined
-- >>> let hand1 = PlayerHand p1

-- | Card value is face value for number cards, 10 for face cards, and 11 for Ace.
toPoints :: Card -> Int
toPoints (Card _ rank) | rank == Ace = 11
                       | rank < Jack = fromEnum rank + 1
                       | -- Face cards are all worth 10
                         otherwise   = 10

-- | Checks if the hand is a Combo hand, defined as two cards with a value of targetValue
--
-- >>> isCombo [Card Heart Ace, Card Heart Jack]
-- True
--
-- >>> isCombo [Card Spade Queen, Card Diamond Ace]
-- True
--
-- >>> isCombo [Card Spade Three, Card Spade Four]
-- False
--
-- >>> isCombo [Card Spade Ace, Card Diamond Seven, Card Diamond Jack]
-- False
isCombo :: Hand -> Bool
isCombo [c1, c2] | toPoints c1 + toPoints c2 == targetValue = True
isCombo _ = False

-- | Calculates value of hand, adjusting for Aces
--
-- >>> cards = [Card Spade Ace, Card Spade Three, Card Heart Five]
-- >>> handValue cards
-- 19P
--
-- >>> cards = [Card Spade Ace, Card Spade Three, Card Heart Nine]
-- >>> handValue cards
-- 13P
--
-- >>> cards = [Card Spade Ace, Card Heart Nine, Card Diamond Queen, Card Spade King]
-- >>> handValue cards
-- Bust
--
-- >>> cards = [Card Spade Ace, Card Heart Ace, Card Diamond Nine]
-- >>> handValue cards
-- 21P
--
-- >>> cards = [Card Spade Ace, Card Heart Ace, Card Diamond Nine, Card Spade King]
-- >>> handValue cards
-- 21P
--
-- >>> cards = [Card Spade Ace, Card Heart Ace, Card Club Ace, Card Diamond Ace]
-- >>> handValue cards
-- 14P
--
-- >>> cards = [Card Spade Ace, Card Spade King]
-- >>> handValue cards
-- Combo
--
-- >>> cards = [Card Spade Ace, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Five]
-- >>> handValue cards
-- Charlie
--
-- >>> cards = [Card Spade Ten, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Five]
-- >>> handValue cards
-- Bust
-- >>> cards = [Card Club Five, Card Club Six, Card Spade Ace]
-- >>> handValue cards
-- 12P
-- >>> cards = [Card Diamond Four,Card Diamond Six]
-- >>> handValue cards
-- 10P
handValue :: Hand -> HandValue
handValue cards | isCombo cards            = Combo
                | value > targetValue      = Bust
                | length cards == maxCards = Charlie
                | otherwise                = Value value
    where value = handCalc cards

-- | Calculate value of a hand
--
-- >>> handCalc [Card Diamond Four,Card Diamond Six]
-- 10
handCalc :: Hand -> Int
handCalc cards = value
  where
    (aces, notAces) = partition ((== 11) . toPoints) cards

    value           = notAcesSum + minAceValue + acesModifier

    notAcesSum      = sum $ toPoints <$> notAces
    minAceValue     = length aces -- Small Ace value is 1
    gap             = targetValue - (notAcesSum + minAceValue)
    acesModifier | gap > 0   = min minAceValue (div gap 10) * 10
                 | otherwise = 0


-- | Evaluate how many points the player receives
--
-- >>> evaluatePoints' 10 Bust Combo
-- 25
--
-- >>> evaluatePoints' 10 (Value 18) (Value 20)
-- 20
--
-- >>> evaluatePoints' 10 (Value 20) (Value 18)
-- 0
evaluatePoints' :: Points -> HandValue -> HandValue -> Points
evaluatePoints' bid Combo Combo = bid

-- Combo gives 2.5x
evaluatePoints' bid _     Combo = round (2.5 * realToFrac bid :: Double)

-- Dealer wins when player busts
evaluatePoints' _   _     Bust  = 0

-- General case
evaluatePoints' bid dealer player | player < dealer  = 0
                                  | player == dealer = bid
                                  | otherwise        = 2 * bid

-- | Evaluate insurance bid using dealer hand value
evaluateInsurance :: Points -> HandValue -> Points
evaluateInsurance bid Combo = 2 * bid
evaluateInsurance _   _     = 0

evaluatePoints :: HandValue -> Play -> Points
evaluatePoints dvalue play
    | isInsurance $ act play = evaluateInsurance bid dvalue
    | otherwise              = evaluatePoints' bid dvalue pvalue
  where
    bid    = getPoints play
    pvalue = handValue . finalHand $ play

findBid :: PlayNode -> Maybe Points
findBid Nil            = Nothing
findBid (PlayNode p r) = case act p of
    Bid n -> Just n
    _     -> findBid r

-- | Verify that the chosen action is a valid play
--
--   - Cannot bid points exceeding current pointsd
--   - Bids are bounded
--   - Cannot bid less than minimum bid unless all in
--   - Double only on first turn
--   - Split only with pairs
--   - Insurance only when up-card is Ace
--   - Insurance bid is always half of initial bid
validPlay
    :: Action         -- Player action
    -> PlayerHand     -- Player's Hand
    -> [PlayerPoints] -- Player's points
    -> Maybe Card     -- Dealer's up-card
    -> PlayNode       -- Current play node
    -> Either GameError Action
validPlay action hand pp (Just (Card _ rank)) node = case action of
    Bid        _ -> err LateBidError
    Insurance  b -> validInsurance pid points rank lastAct b
    DoubleDown b -> validDoubleDown pid points bid cs b
    Split      b -> validSplit pid points bid cs b
    _            -> validSequence pid action node
  where
    pid     = getId hand
    err     = Left . flip GameError pid
    cs      = cards hand
    points  = find' getPoints pid getId pp
    lastAct = act $ nodeValue' node
    bid     = fromJust $ findBid node

-- Bid actions (this is stil part of validPlay)
--
-- >>> validPlay (Bid minBid) (hand1 []) [pp1 (minBid + 1)] Nothing []
-- Right (Bid 10)
--
-- >>> validPlay (Bid minBid) (hand1 []) [pp1 (minBid - 1)] Nothing []
-- Left Error: '1' Insufficient points for bid
--
-- >>> validPlay (Bid (minBid - 1)) (hand1 []) [pp1 (minBid - 1)] Nothing []
-- Right (Bid 9)
--
-- >>> validPlay Hit (hand1 []) [pp1 (minBid - 1)] Nothing []
-- Left Error: '1' Must place a bid
--
-- >>> validPlay (Bid 5) (hand1 [s1, s2]) [pp1 (minBid - 1)] (Just d5) []
-- Left Error: '1' Bids can only be placed at the start of the hand
validPlay action@(Bid bid) hand pp Nothing _
    | -- Allow all in
      bid < minBid && cpoints >= minBid = err MinBidError
    | bid > maxBid                      = err MaxBidError
    | bid > cpoints                     = err BidError
    | cpoints == 0                      = err MinBidError
    | otherwise                         = Right action
  where
    err     = Left . flip GameError (getId hand)
    cpoints = find' getPoints (getId hand) getId pp

-- Must place bid in first turn
validPlay _ hand _ Nothing _ = Left $ GameError NoBidError (getId hand)

-- | Check that the insurance action is valid
--
-- >>> validInsurance "1" 10 Ace (Bid 10) 5
-- Right (Insurance 5)
--
-- >>> validInsurance "1" 0 Ace (Bid 10) 5
-- Left Error: '1' Insurance bid with insufficient points
--
-- >>> validInsurance "1" 10 Two (Bid 10) 5
-- Left Error: '1' Insurance bid when dealer's up-card is not Ace
--
-- >>> validInsurance "1" 10 Ace Hit 10
-- Left Error: '1' Called insurance after first turn
validInsurance
    :: PlayerId
    -> Points
    -> Rank
    -> Action
    -> Points
    -> Either GameError Action
validInsurance pid cpoints uprank (Bid bid) b
    | maxInsure bid /= b      = err $ DeclaredBidError b (maxInsure bid)
    | maxInsure bid > cpoints = err InsuranceBidError
    | uprank /= Ace           = err InsuranceError
    | otherwise               = Right (Insurance b)
    where err = Left . flip GameError pid

-- Can only call insurance right after a bid play
validInsurance pid _ _ _ _ = Left $ GameError LateInsuranceError pid

-- | Check that the split action is valid
--
-- >>> validSplit "1" 10 5 [s1, h1] 5
-- Right (Split 5)
--
-- >>> validSplit "1" 5 10 [s1, h1] 10
-- Left Error: '1' Split with insufficient points
--
-- >>> validSplit "1" 10 5 [s1, h2] 5
-- Left Error: '1' Split without a pair
validSplit
    :: PlayerId
    -> Points
    -> Points
    -> Hand
    -> Points
    -> Either GameError Action
validSplit pid cpoints bid [c1, c2] b
    | bid > cpoints            = err SplitBidError
    | b /= bid                 = err $ DeclaredBidError b bid
    | getRank c1 /= getRank c2 = err SplitError
    | otherwise                = Right (Split b)
    where err = Left . flip GameError pid

-- Can only split on two cards
validSplit pid _ _ _ _ = Left $ GameError SplitError pid

-- | Check that the double down action is valid
--
-- >>> validDoubleDown "1" 10 5 [d8, s2] 5
-- Right (DoubleDown 5)
validDoubleDown
    :: PlayerId
    -> Points
    -> Points
    -> Hand
    -> Points
    -> Either GameError Action
validDoubleDown pid cpoints bid [_, _] b
    | b /= bid      = err $ DeclaredBidError b bid
    | bid > cpoints = err DoubleDownBidError
    | otherwise     = Right (DoubleDown b)
    where err = Left . flip GameError pid

-- Can only double down on two cards
validDoubleDown pid _ _ _ _ = Left $ GameError LateDoubleDownError pid

-- | Check that the sequence of actions is valid.
--
--  - Double down must follow a certain sequence
--  - [[DoubleDown, Hit, Stand], [DoubleDown, Hit], [DoubleDown]]
--
-- Testing tricks
-- >>> let play a = Play "1" 0 5 a "" []
-- >>> let p0 = PlayNode (play (Bid 10)) Nil
-- >>> let p1 = PlayNode (play (DoubleDown 1)) p0
-- >>> let p2 = PlayNode (play Hit) p1
-- >>> let p3 = PlayNode (play Hit) p2
--
-- >>> validSequence "1" (DoubleDown 1) p0
-- Right (DoubleDown 1)
--
-- >>> validSequence "1" Hit p1
-- Right Hit
--
-- >>> validSequence "1" Stand p2
-- Right Stand
--
-- >>> validSequence "1" (DoubleDown 1) p3
-- Right (DoubleDown 1)
--
-- >>> validSequence "1" Stand p1
-- Left Error: '1' Incorrect sequence of actions for double down [DoubleDown 1,Stand]
validSequence :: PlayerId -> Action -> PlayNode -> Either GameError Action
validSequence pid action node | hasDouble && wrongSequence = doubleError
                              | otherwise                  = Right action
  where
    err               = Left . flip GameError pid
    doubleError       = err $ DoubleDownError (show lastDoubleActions)

    -- Previous play nodes
    prevNode          = nextNode node
    last2Actions      = act <$$> [nodeValue =<< prevNode, nodeValue node]

    -- Previous actions (actual action order is reverse of node order)
    last3Actions      = catMaybes last2Actions ++ [action]
    lastDoubleActions = dropWhile (not . isDoubleDown) last3Actions

    -- Sequences only care about the constructor
    validDoubleSequences =
        [ [DoubleDown (-1), Hit, Stand]
        , [DoubleDown (-1), Hit]
        , [DoubleDown (-1)]
        ]

    -- Checks
    hasDouble     = any isDoubleDown last3Actions
    wrongSequence = lastDoubleActions `notElem` validDoubleSequences

-- | Find using playerId
find' :: (a -> Points) -> PlayerId -> (a -> PlayerId) -> [a] -> Points
find' f playerId conv t = maybe (-1) f (find ((== playerId) . conv) t)

-- | Rules for dealer's play
--
--  - Hit on 16
dealerPlayFunc :: Hand -> Action
dealerPlayFunc cards | handCalc cards <= 16 = Hit
                     | otherwise            = Stand

checkMemory :: String -> PlayerId -> Either GameError String
checkMemory mem pid
    | length mem > (10 ^ (4 :: Int)) = Left $ GameError MemoryError pid
    | otherwise                      = Right mem
