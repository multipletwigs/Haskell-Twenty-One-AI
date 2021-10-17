-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game

-- You can add more imports if you need them

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard dCard p pinf pid myMem hand
   -- trace ("dCard: " ++ show dCard ++  "pid= " ++ show pid ++ " pinfo: " ++ show pinf ++ " hand: " ++ show hand ++ " memory: " ++ show myMem) False = undefined
    |otherwise = playStrat dCard myMem hand 
    

playStrat :: Maybe Card -> Maybe String -> Hand -> (Action, String)
playStrat Nothing (Just myMem) _ = (Bid maxBid, myMem ++ "B")
playStrat (Just dealerCard) (Just (x:myMem)) myCards
    | (handCalc myCards) > 17 = (Stand, x:myMem ++ "S")
    | length myCards == 2 = (DoubleDown 200, x:myMem ++ "D")
    | (Ace == getRank dealerCard) && (x /= 'B') = (Insurance 50, x:myMem ++ "I")
    | otherwise = (Hit, x:myMem ++ "H") 
