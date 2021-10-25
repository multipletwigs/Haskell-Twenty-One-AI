module Main where

import           EitherIO
import           TwentyOne.Play
import           TwentyOne.Types
import           TwentyOne.Rules
import           Utils

import           Data.List
import           Data.Function
import           Data.Ord

import           Control.Monad

import safe      Player

-- This sets up a tournament with instances of your player playing against
-- each other.  You can run different players against each other, but you'll
-- need to change the Module names of those players (don't forget to change the
-- module name back to "Player" when you submit your code)
players :: [Player]
players = flip Player Player.playCard . show <$> ([0 .. 9] :: [Int])

rankings
    :: EitherIO GameError GameResult
    -> EitherIO GameError [(PlayerId, PointState)]
rankings g = do
    (GameResult _ scores _ _) <- g
    let r    = finalPoints <$> scores
        pids = getId . player <$> scores
    return $ zip pids r

games :: [EitherIO GameError GameResult]
games | length players > 2 = repeat $ playGame startingPoints players
      | otherwise          = error "Must have at least 3 players"

-- |
--
-- >>> length (occurances ["1", "2", "3", "1", "2"]) == length (players)
-- True
occurances :: [PlayerId] -> [Wins]
occurances pid = sortOn winId (wins ++ (allPids \\ wins))
  where
    toWins  = (liftM2 Wins head length <$>)
    wins    = toWins . group $ sort pid
    allPids = flip Wins 0 . getId <$> players

data Wins = Wins {winId :: PlayerId, wins:: Int}

instance Show Wins where
    show (Wins wid w) = show (wid, w)

instance Eq Wins where
    (Wins w1 _) == (Wins w2 _) = w1 == w2

data Ranking = Ranking {_rankId :: PlayerId, first:: Int, second:: Int, third :: Int}

instance Identify Ranking where
    getId = _rankId

instance Show Ranking where
    show (Ranking rid f s t) =
        "Player: "
            ++ show rid
            ++ ", "
            ++ show f
            ++ " | "
            ++ show s
            ++ " | "
            ++ show t
            ++ " , "
            ++ show (f + s + t)

results :: Wins -> Wins -> Wins -> Ranking
results (Wins id1 f) (Wins id2 s) (Wins id3 t)
    | not $ all (== id1) [id1, id2, id3]
    = error $ "IDs not the same for result zip" ++ show
        [(id1, f), (id2, s), (id3, t)]
    | otherwise
    = Ranking id1 f s t

printResult
    :: (Integer, EitherIO GameError GameResult)
    -> EitherIO GameError GameResult
printResult (i, p) = do
    (GameResult a scores c d) <- p
    liftIO . putStrLn $ "=======" ++ show i ++ "======="
    liftIO $ forM_ scores print
    return $ GameResult a scores c d

rankingWeight :: Ranking -> Int
rankingWeight (Ranking _ f s t) = f * 3 + s * 2 + t

playMultipleGames :: Int -> EitherIO GameError [Ranking]
playMultipleGames n = do
    let g = take n $ zip [1 ..] games
    r <- mapM (rankings . printResult) g
    let resultsSorted          = sortOn (Down . snd) <$> r
        top3                   = take 3 <$> resultsSorted
        [first, second, third] = occurances <$> transpose (fst <$$> top3)
        ranked                 = zipWith3 results first second third
    return $ sortOn (Down . rankingWeight) ranked

mainMultiple :: Int -> IO ()
mainMultiple n = do
    played <- runEitherIO $ playMultipleGames n
    case played of
        Right r -> do
            putStrLn "=============="
            forM_ r print
        Left e -> print e

mainSingle :: IO ()
mainSingle = do
    played <- runEitherIO $ head games
    case played of
        Right (GameResult hr scores _ _) -> do
            forM_ (reverse hr) print
            putStrLn "=============="
            forM_ scores print
        Left e -> print e

main :: IO ()
-- main = mainMultiple 100
main = mainSingle
