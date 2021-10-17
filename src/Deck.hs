module Deck where

import           Cards
import           Control.Monad
import           Data.List
import           System.Random

-- | Deal n cards each to m players.
deal :: Int -> Int -> [Card] -> [Hand]
deal n m = take m . map (take n) . iterate (drop n)

-- | Assume n * m < length of the deck, to avoid nasty recursion.
--
-- >>> length . concat . fst <$> dealContinuous 3 2 5 sortedDeck
-- 10
--
-- >>> length . concat . fst <$> dealContinuous 3 0 0 sortedDeck
-- 0
--
-- >>> length . concat . fst <$> dealContinuous 3 0 1 sortedDeck
-- 0
--
-- >>> length . concat . fst <$> dealContinuous 3 1 0 sortedDeck
-- 0
--
-- >>> length . concat . fst <$> dealContinuous 3 1 1 sortedDeck
-- 1
--
-- >>> length . concat . fst <$> dealContinuous 3 53 1 sortedDeck
-- 53
--
-- >>> length . concat . fst <$> dealContinuous 3 100 1 sortedDeck
-- 100
--
-- >>> length . concat . fst <$> dealContinuous 3 2 100 sortedDeck
-- 200
dealContinuous :: Int -> Int -> Int -> [Card] -> IO ([Hand], [Card])
dealContinuous decksUsed n m deck = do
    -- Take from deck
    let total       = m * n
        numFromDeck = min total (length deck)
        dealtCards  = take numFromDeck deck
        deck'       = drop numFromDeck deck
        remaining   = total - numFromDeck

    -- Take more from deck after shuffling
    newDeck <- genDeck deck'
    let allDealtCards = dealtCards ++ take remaining newDeck
        newDeck'      = drop remaining newDeck
        finalDeal     = deal n m allDealtCards

    return (finalDeal, newDeck')

  where
    genDeck :: [Card] -> IO [Card]
    genDeck [] = shuffledDecks decksUsed
    genDeck d  = return d

-- | Take a number of cards from the deck, with reshuffling. Assume n << size of deck.
--
-- >>> length . fst <$> takeContinuous 3 0 sortedDeck
-- 0
--
-- >>> length . fst <$> takeContinuous 3 1 sortedDeck
-- 1
--
-- >>> length . fst <$> takeContinuous 3 10 sortedDeck
-- 10
--
-- >>> length . fst <$> takeContinuous 3 52 sortedDeck
-- 52
--
-- >>> length . fst <$> takeContinuous 3 53 sortedDeck
-- 53
--
-- >>> length . fst <$> takeContinuous 3 100 sortedDeck
-- 100
--
-- >>> sumDeck = sum . (fromEnum . getRank <$>)
-- >>> liftM2 (==) (sumDeck . fst <$> (shuffledDecks 3 >>= takeContinuous 3 (52*3))) (sumDeck <$> shuffledDecks 3)
-- True
--
-- >>> length . snd <$> (shuffledDecks 3 >>= takeContinuous 3 (52*3))
-- 156
takeContinuous :: Int -> Int -> [Card] -> IO ([Card], [Card])
takeContinuous decksUsed n cards = do
    (c, d) <- dealContinuous decksUsed n 1 cards
    return (concat c, d)

sortedDeck :: [Card]
sortedDeck = Card <$> [Spade ..] <*> [Ace ..]

shuffleList :: [a] -> IO [a]
shuffleList l = do
    i <- replicateM (length l) (randomIO :: IO Int)
    return $ map snd $ sortOn fst $ zip i l

-- |
-- >>> sumDeck = sum . (fromEnum . getRank <$>)
--
-- >>> (== sumDeck sortedDeck) . sumDeck <$> (shuffledDecks 1)
-- True
--
-- >>> (== sumDeck (sortedDeck ++ sortedDeck)) . sumDeck <$> (shuffledDecks 2)
-- True
shuffledDecks :: Int -> IO [Card]
shuffledDecks = shuffleList . (sortedDeck >>=) . replicate
