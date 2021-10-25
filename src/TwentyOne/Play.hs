{-# LANGUAGE NamedFieldPuns #-}
module TwentyOne.Play where

import           Data.Function
import           Data.List
import           Data.Maybe
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           System.Timeout
import           Data.Bits

-- Base modules
import           Cards
import           Deck
import           Utils
import           EitherIO

-- Game modules
import           TwentyOne.Rules
import           TwentyOne.Types

-- | Rounds until the game ends
roundLimit :: Int
roundLimit = 100

dealerId :: PlayerId
dealerId = "dealer"

-- Play game

-- | Play out a full game, then update the players.
playGame
    :: Points    -- ^ max score for the game
    -> [Player] -- ^ a list of players
    -> EitherIO GameError GameResult
playGame _ players = do
    liftIO $ print "Starting game"

    -- Initial game state with random ordering and shuffled decks
    let nilScore = map (`GamePoints` Rich startingPoints) players
    nilScore' <- liftIO $ shuffleList nilScore
    let nilResult = GameResult [] nilScore' players (shuffledDecks numDecks)

    (GameResult played results updatedPlayers newDeck) <- playUntil
        roundLimit
        nilResult

    return $ GameResult played results updatedPlayers newDeck

-- | Play hands until we reach the max rounds, or all players bankrupt.
playUntil :: Int -> GameResult -> EitherIO GameError GameResult
playUntil currentRound results
    | currentRound <= 0 = return results
    | all isBankrupt points = return results
    | otherwise = playRound results >>= playUntil (currentRound - 1)
    where points = finalPoints <$> gamePoints results


updatePoints :: Int -> GamePoints -> HandPoints -> GamePoints
updatePoints count (GamePoints p c) h
    | (not . isBankrupt) c && getPoints h == 0 = GamePoints p (Bankrupt count)
    | otherwise = case c of
        Rich 0 -> GamePoints p (Bankrupt count)
        Rich _ -> GamePoints p (Rich $ getPoints h)
        b      -> GamePoints p b


-- | Shuffle a deck then start the game, keep track of the score.
playRound :: GameResult -> EitherIO GameError GameResult
playRound (GameResult previous results players shuffledDeck) = do
    -- Lift and then extract deck
    newDeck                         <- liftIO shuffledDeck

    (results', tricked, _, stocked) <- playHand newDeck prevTrick results

    -- Replace point values of 0 with Bankrupt
    let hp    = handPoints results'
        count = length $ filter (isBankrupt . finalPoints) results
        newGP = zipWith (updatePoints count)
                        (sortOn getId results)
                        (sortOn getId hp)

    -- Restore ordering
    let newGP' = sortAlong getId (getId <$> results) newGP

    return $ GameResult (HandResult tricked hp : previous)
                        newGP'
                        players
                        (pure stocked)
  where
    (prevTrick, _) = case previous of
        []    -> ([], [])
        p : _ -> (tricks p, handPoints p)

dealerPlay :: Action -> Hand -> Play
dealerPlay a = Play dealerId 0 0 a ""

noPlayDealer :: [Card] -> Hand -> (HandValue, [Card], PlayNode)
noPlayDealer stock dealerCards = (Value (-1), stock, noPlay)
    where noPlay = PlayNode (dealerPlay Stand [last $ init dealerCards]) Nil

-- | Dealer's turn
playDealerHand :: Stock -> Hand -> IO (HandValue, Stock, PlayNode)
playDealerHand stock cards = do
    (dealerTrick, stocked) <- dealerPlayFunc' stock cards Nil
    let dealerHand  = finalHand $ nodeValue' dealerTrick
        dealerValue = handValue dealerHand

    return (dealerValue, stocked, dealerTrick)

-- | Auxiliary function used to play for the dealer
dealerPlayFunc' :: Stock -> Hand -> PlayNode -> IO (PlayNode, Stock)
dealerPlayFunc' stock c tricks = do
    (newCard, stocked) <- takeContinuous numDecks 1 stock
    let s = head newCard
    case dealerPlayFunc c of
        Hit -> dealerPlayFunc' stocked
                               (s : c)
                               (PlayNode (dealerPlay Hit (s : c)) tricks)
        Stand -> return (PlayNode (dealerPlay Stand c) tricks, stock)
        _     -> error "Dealer should not use other Actions"

playBids
    :: Trick
    -> Stock
    -> [GamePoints]
    -> [Player]
    -> EitherIO GameError (Trick, Stock, [GamePoints])
playBids prev stock rich = foldl f' (pure ([], stock, rich))
  where
    f' r p = do
        (t, s, gp) <- r
        playCards Nothing gp s prev t (PlayerHand p []) 1

combinePoints :: [HandPoints] -> [GamePoints] -> [Points]
combinePoints handPoints newGP = zipWith (+) finalPoints sortedGP
  where
    groups      = groupEqual getId (sortOn getId handPoints)
    finalPoints = sum <$> (getPoints <$$> groups)
    sortedGP    = getPoints <$> sortOn getId newGP

evaluatePlays :: Trick -> Stock -> Hand -> IO ([HandPoints], PlayNode, Stock)
evaluatePlays tricked stock dealerCards = do
    -- Get only the player hands
    let playerPlays =
            filter (not . isInsurance . act) (nodeValue' <$> tricked)

    -- Evaluate value of each hand
    let phv          = handValue . finalHand <$> playerPlays

    -- Play dealer
    let noPlayResult = noPlayDealer stock dealerCards

    (dealerValues, stock', dealerTrick) <- if all isBust phv
        then pure noPlayResult
        else playDealerHand stock dealerCards

    let finalPoints = calculatePoints (nodeValue' <$> tricked) dealerValues

    return (finalPoints, dealerTrick, stock')

calculatePoints :: [Play] -> HandValue -> [HandPoints]
calculatePoints plays dvalue = zipWith HandPoints points ids
  where
    points = liftA2 evaluatePoints (repeat dvalue) plays
    ids    = getId <$> plays

-- | Distribute a (shuffled) deck to the players and start the game.
--   Assume we always have sufficient amount of cards.
playHand
    :: [Card]
    -> Trick
    -> [GamePoints]
    -> EitherIO GameError (HandResult, Trick, Card, Stock)
playHand deck prev scores = do
    -- Deal cards, with shuffling
    (dealt, stock) <- liftIO
        $ dealContinuous numDecks startingNumCards (length scores + 1) deck
    let dealerCards : playerCards = dealt

    -- Separate rich and bankrupt players
    let (rich, bankrupt) = partition (isRich . finalPoints) scores
        order            = player <$> rich

    -- Pre play - bids
    (bidTrick, stock', bidGP  ) <- playBids prev stock rich order

    -- Main play
    (tricked , newGP , stock'') <- playTricks
        (zipWith PlayerHand order playerCards)
        stock'
        dealerCards
        prev
        bidTrick
        bidGP

    (handPoints, dealerTrick, stock''') <- liftIO
        $ evaluatePlays tricked stock'' dealerCards

    -- Combine players
    let resultPoints   = combinePoints handPoints newGP
        pids           = sort $ getId <$> order
        zeroPlayers    = HandPoints 0 . getId <$> bankrupt
        nonZeroPlayers = zipWith HandPoints resultPoints pids
        players        = nonZeroPlayers ++ zeroPlayers
        results        = HandResult tricked players

    return (results, dealerTrick : tricked, head dealerCards, stock''')


-- | Play out a round one hand at a time until everyone either busts, stands, or has TwentyOne.
playTricks
    :: [PlayerHand]   -- List of player hands, each starting with 2 cards
    -> Stock          -- Stock pile
    -> Hand           -- Dealer hand
    -> Trick          -- Previous trick
    -> Trick          -- Current trick
    -> [GamePoints]   -- Current players' points
    -> EitherIO GameError (Trick, [GamePoints], Stock) -- New Trick, New Scores, Updated Stock Pile
playTricks [] stock _ _ tricks points = return (tricks, points, stock)
playTricks (hand : otherHands) stock dealersHands prev current scores
    |
    -- Stop execution after 10000 tricks
      length current > 10000 = error "10000 tricks reached"
    | otherwise = do
        -- Put previous memory when playing
        (played, stocked, scored) <- playCards (listToMaybe dealersHands)
                                               scores
                                               stock
                                               prev
                                               current
                                               hand
                                               1

        -- Play trick for rest of the players
        playTricks otherHands stocked dealersHands prev played scored

parseAction
    :: Action -> Hand -> [Card] -> Points -> IO (Hand, [Card], Points, Points)
parseAction action hand stock bid = do
    (h, t) <- takeContinuous numDecks 1 stock
    return $ case action of
        Hit            -> (head h : hand, t, bid, 0)
        (Split     _)  -> (hand, stock, bid, bid)
        (Insurance _)  -> (hand, stock, bid, maxInsure bid)
        Bid b          -> (hand, stock, bid + b, b)
        (DoubleDown _) -> (hand, stock, bid * 2, bid)
        Stand          -> (hand, stock, bid, 0)

playAction
    :: Maybe Card
    -> PlayerHand
    -> [GamePoints]
    -> Trick
    -> Trick
    -> PlayNode
    -> Int
    -> EitherIO GameError (Action, String)
playAction upCard hand scores prev current node sid' = do
    let (PlayerHand Player { _playerId = pid, playFunc } handCards) = hand

    -- Process information for player
    let pp         = gamePointsToPlayerPoints <$> scores
        infos      = combineWith (playToInfo . nodeValue') current prev
        userMemory = (firstJust `on` getMemory pid sid') current prev

    -- Execute user function
    (choice', raw) <- timeCall (playFunc upCard pp infos pid userMemory)
                               pid
                               handCards

    -- Check action is valid
    updated <- liftEither $ checkMemory raw pid
    -- state   <- liftEither $ checkMemory updated
    choice  <- liftEither $ validPlay choice' hand pp upCard node

    return (choice, updated)

-- | Returns (sid1, sid2)
nextSids :: Int -> (Int, Int)
nextSids n = (n << 1, n << 1 + 1)

-- | Trim trailing zeros from sid
--
-- prop>\n m -> trimSid (n << m) == n
trimSid :: Int -> Int
trimSid 0   = 0
trimSid sid = sid `div` ((-sid) .&. sid)

-- | Call the play function of a player, updates the stock and player's hand.
playCards
    :: Maybe Card    -- Dealer's current up-card
    -> [GamePoints]  -- Scores from previous round
    -> Stock         -- Stock pile
    -> Trick         -- Previous trick
    -> Trick         -- Current trick
    -> PlayerHand    -- Player hand
    -> Int           -- Second ID used to identify separate Play sequences
    -> EitherIO GameError (Trick, Stock, [GamePoints]) -- new trick, new stock pile, new points
playCards upCard scores stock prev current hand sid = do
    let (PlayerHand Player { _playerId = pid } handCards) = hand
        sid' = trimSid sid  -- Remove trailing zeros, used for searching in trick

    -- Player's previous action in the current round
    let playerNode =
            fromMaybe Nil $ find (hasIds pid sid' . nodeValue') current

    -- Player's action
    (choice, updated) <- playAction upCard
                                    hand
                                    scores
                                    prev
                                    current
                                    playerNode
                                    sid'

    -- Parse changes from action
    (newCards, stocked, updatedBid, newBid) <- liftIO
        $ parseAction choice handCards stock (getPoints playerNode)

    -- Combine with results
    let newScores = map' ((== pid) . getId) (gpmap (`minus` newBid)) scores

    -- Update trick with action
    let newPlay s = Play pid s updatedBid choice updated newCards
        newCurrent = update (PlayNode (newPlay sid') playerNode) current

    -- What to do next
    let nextPlayCard s' st' ph' i' t' =
            playCards upCard s' st' prev t' (PlayerHand (owner hand) ph') i'

        continuePlaying = nextPlayCard newScores stocked newCards

    -- Auxiliary function for pattern matching to determine recursion
    let playCards'
            :: Action
            -> HandValue
            -> EitherIO GameError (Trick, Stock, [GamePoints])
        playCards' Hit            (Value _) = continuePlaying sid newCurrent
        playCards' (DoubleDown _) (Value _) = continuePlaying sid newCurrent
        playCards' (Insurance  _) _         = do
            let (sid1, sid2) = nextSids sid
                insNewplay   = newPlay sid2
                insTree      = PlayNode insNewplay playerNode : newCurrent
            continuePlaying sid1 insTree

        playCards' a@(Split _) _ = do
            (cards', stocked') <- liftIO $ takeContinuous numDecks 2 stocked

            -- Mix cards in hand with new cards
            let [hand1, hand2] = transpose [handCards, cards']
                (sid1 , sid2 ) = nextSids sid

            -- Original line
            (t', s', p') <- nextPlayCard newScores
                                         stocked'
                                         hand1
                                         sid1
                                         newCurrent

            -- Find memory from original line
            let mem' = fromJust $ getMemory pid (trimSid sid1) t'

            -- Split line
            let splitNewPlay = Play pid sid2 updatedBid a mem' handCards
                splitTree    = PlayNode splitNewPlay playerNode : t'

            nextPlayCard p' s' hand2 sid2 splitTree

        playCards' _ _ = return (newCurrent, stocked, newScores)

    playCards' choice (handValue newCards)

getMemory :: PlayerId -> Int -> Trick -> Maybe String
getMemory _   _   [] = Nothing
getMemory pid sid x  = field $ nodeValue' <$> x
    where field = fmap memory . find (hasIds pid sid)

-- | Generic function for calling player functions with a timer
timeCall :: NFData b => (Hand -> b) -> PlayerId -> Hand -> EitherIO GameError b
timeCall func pid handCards = EitherIO $ do
  -- Careful, these are microsecs
    played <- timeout 1000000 $ return $!! func handCards -- Will force evaluation
    let timed = case played of
            Nothing -> Left $ GameError TimeError pid
            Just c  -> Right c
    return timed
