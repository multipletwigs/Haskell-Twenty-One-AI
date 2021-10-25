{-# LANGUAGE NamedFieldPuns  #-}

-- | Datatypes for playing TwentyOne
module TwentyOne.Types where

import           Cards
import           Control.DeepSeq
import           Control.Monad
import           Data.Function

{-
    Basic types
-}

type Points = Int

type PlayerId = String

{-
    Typeclasses
-}

class Identify a where
    getId :: a -> PlayerId

class Pointable a where
    getPoints :: a -> Points

{-
    Game types
-}

data Player = Player {
  _playerId :: PlayerId,
  playFunc :: PlayFunc
}

instance Identify Player where
    getId = _playerId

instance Eq Player where
    Player { _playerId = a } == Player { _playerId = b } = a == b

instance Show Player where
    show a = "Player: " ++ show (getId a)

-- | Play function type.
type PlayFunc
    =  Maybe Card         -- Dealer's up-card
    -> [PlayerPoints]     -- Points for all players in game
    -> [PlayerInfo]       -- the most recent information for all players
    -> PlayerId           -- the player's id
    -> Maybe String       -- the player's memory
    -> Hand               -- the player's hand
    -> (Action, String)   -- the player's chosen action and new memory

-- | Player that is currently playing
data PlayerHand = PlayerHand {
    owner :: Player,
    cards :: Hand
}

instance Identify PlayerHand where
    getId = getId . owner

data PlayerInfo = PlayerInfo {
    _playerInfoId :: PlayerId,
    playerInfoHand :: Hand
}

instance Eq PlayerInfo where
    (==) = (==) `on` getId

instance Identify PlayerInfo where
    getId = _playerInfoId

instance Show PlayerInfo where
    show (PlayerInfo pid h) = show pid ++ " " ++ show h

data Action = Bid Points | Hit | Stand | DoubleDown Points | Split Points | Insurance Points
    deriving(Show)

instance Eq Action where
    (==) (Bid b1)       (Bid b2)       = b1 == b2
    (==) Hit            Hit            = True
    (==) Stand          Stand          = True
    (==) (Split      _) (Split      _) = True
    (==) (DoubleDown _) (DoubleDown _) = True
    (==) (Insurance  _) (Insurance  _) = True
    (==) _              _              = False

instance NFData Action where
    rnf = rwhnf

data Play = Play
  { _playId :: PlayerId
  , secondId :: Int
  , _playBid :: Points
  , act :: Action
  , memory :: String
  , finalHand :: Hand
  }

instance Identify Play where
    getId = _playId

instance Pointable Play where
    getPoints = _playBid

instance Show Play where
    show Play { _playId, secondId, act, _playBid, finalHand } =
        "("
            ++ show _playId
            ++ ","
            ++ show secondId
            ++ ","
            ++ show _playBid
            ++ ","
            ++ show act
            ++ ","
            ++ show finalHand
            ++ ")"

instance Eq Play where
    p1 == p2 = getId p1 == getId p2 && secondId p1 == secondId p2

hasIds :: PlayerId -> Int -> Play -> Bool
hasIds pid sid = liftM2 (&&) ((== pid) . getId) ((== sid) . secondId)

{-
    Collections
-}

-- | Cards that can be drawn
type Stock = [Card]

-- | Represents what happened in one round of a game
type Trick = [PlayNode]

-- | Linked list of Plays for a particular Player. Should be used in conjunction with a list of active Plays.
--
-- !Note: The PlayNode for a Dealer is different. The hand in the Play represents the cards that  can be seen by players
data PlayNode = PlayNode Play PlayNode | Nil

instance Pointable PlayNode where
    getPoints Nil            = 0
    getPoints (PlayNode p _) = getPoints p

instance Eq PlayNode where
    Nil             == Nil             = True
    (PlayNode p1 _) == (PlayNode p2 _) = p1 == p2
    _               == _               = False

instance Show PlayNode where
    show Nil            = "Nil"
    -- show (PlayNode p c) = show c ++ "->" ++ show p
    show (PlayNode p c) = show p

nodeValue :: PlayNode -> Maybe Play
nodeValue (PlayNode p _) = Just p
nodeValue _              = Nothing

nodeValue' :: PlayNode -> Play
nodeValue' (PlayNode p _) = p
nodeValue' _              = error "Tried to get value of Nil"

nextNode :: PlayNode -> Maybe PlayNode
nextNode (PlayNode _ c@(PlayNode _ _)) = Just c
nextNode _                             = Nothing

{-
    Error management
-}

data GameError = GameError PlayerError PlayerId

instance Show GameError where
    show (GameError err pid) = "Error: '" ++ pid ++ "' " ++ show err

data PlayerError = TimeError
                 | MemoryError
                 | DeclaredBidError Points Points
                 | BidError
                 | NoBidError
                 | LateBidError
                 | MaxBidError
                 | MinBidError
                 | SplitError
                 | SplitBidError
                 | LateDoubleDownError
                 | DoubleDownError String
                 | DoubleDownBidError
                 | InsuranceError
                 | LateInsuranceError
                 | InsuranceBidError

-- | Default text for errors
instance Show PlayerError where
    show TimeError   = "Took too long to play"
    show MemoryError = "Memory is too large"
    show (DeclaredBidError b b') =
        "Declared bid ("
            ++ show b
            ++ ") does not match expected bid ("
            ++ show b'
            ++ ")"
    show BidError            = "Insufficient points for bid"
    show NoBidError          = "Must place a bid"
    show LateBidError = "Bids can only be placed at the start of the hand"
    show MinBidError = "Cannot bid less than minimum amount unless all in"
    show MaxBidError         = "Cannot bid more then maximum amount"
    show SplitError          = "Split without a pair"
    show SplitBidError       = "Split with insufficient points"
    show LateDoubleDownError = "Doubled after first turn"
    show (DoubleDownError s) =
        "Incorrect sequence of actions for double down " ++ s
    show DoubleDownBidError = "Doubled with insufficient points"
    show LateInsuranceError = "Called insurance after first turn"
    show InsuranceError     = "Insurance bid when dealer's up-card is not Ace"
    show InsuranceBidError  = "Insurance bid with insufficient points"

{-
    Results
-}

data GameResult = GameResult {
    hands :: [HandResult],
    gamePoints :: [GamePoints],
    updatedPlayers :: [Player],
    gameDeck :: IO [Card]
}

data HandResult = HandResult {
    tricks :: Trick,           -- One play at a time
    handPoints :: [HandPoints] -- Points per player
}

instance Show HandResult where
    show (HandResult _ points) = show points

data HandPoints = HandPoints {
    _points :: Points,
    _pointsId :: PlayerId
}

instance Pointable HandPoints where
    getPoints = _points

instance Identify HandPoints where
    getId = _pointsId

instance Show HandPoints where
    show HandPoints { _pointsId, _points } =
        show _pointsId ++ ": " ++ show _points

data GamePoints = GamePoints {
    player :: Player,
    finalPoints :: PointState
}

instance Pointable GamePoints where
    getPoints = getPoints . finalPoints  -- This is also not recursion cc:

instance Identify GamePoints where
    getId = getId . player  -- This is not recursive c:

instance Show GamePoints where
    show (GamePoints Player { _playerId } score) =
        "Player: " ++ show _playerId ++ ", final score: " ++ show score

instance Eq GamePoints where
    (==) = (==) `on` getId

gpmap :: (Points -> Points) -> GamePoints -> GamePoints
gpmap _ gp@(GamePoints _ (Bankrupt _ )) = gp
gpmap f (   GamePoints p (Rich     fp)) = GamePoints p $ Rich (f fp)

-- | Points of a user
data PointState = Bankrupt Int | Rich Points
    deriving (Show, Eq, Ord)

instance Pointable PointState where
    getPoints (Rich     p) = p
    getPoints (Bankrupt _) = error "Should only get points for Rich"


data PlayerPoints = PlayerPoints {
    _playerPointsId :: PlayerId,
    _playerPoints :: Points
}

instance Pointable PlayerPoints where
    getPoints = _playerPoints

instance Identify PlayerPoints where
    getId = _playerPointsId

instance Show PlayerPoints where
    show (PlayerPoints pid p) = "(" ++ show pid ++ " " ++ show p ++ ")"

-- | Result of a hand
data HandValue = Bust | Value Points | Charlie | Combo
    deriving (Eq, Ord)

instance Show HandValue where
    show Bust      = "Bust"
    show Charlie   = "Charlie"
    show Combo     = "Combo"
    show (Value p) = show p ++ "P"

{-
    Convenience conversion functions.
-}

gamePointsToPlayerPoints :: GamePoints -> PlayerPoints
gamePointsToPlayerPoints = liftM2 PlayerPoints getId getPoints

playToInfo :: Play -> PlayerInfo
playToInfo = liftM2 PlayerInfo getId finalHand

{-
    Boolean functions
-}

isBid :: Play -> Bool
isBid Play { act = Bid _ } = True
isBid _                    = False

isSplit :: Action -> Bool
isSplit (Split _) = True
isSplit _         = False

isDoubleDown :: Action -> Bool
isDoubleDown (DoubleDown _) = True
isDoubleDown _              = False

isInsurance :: Action -> Bool
isInsurance (Insurance _) = True
isInsurance _             = False

isBankrupt :: PointState -> Bool
isBankrupt (Bankrupt _) = True
isBankrupt _            = False

isRich :: PointState -> Bool
isRich (Rich a) = a > 0
isRich _        = False

isBust :: HandValue -> Bool
isBust Bust = True
isBust _    = False
