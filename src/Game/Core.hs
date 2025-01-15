module Game.Core 
    ( HandState(..)
    , RoundResult(..)
    , HandResult(..)
    , Player(..)
    , analyzeRounds
    ) where

import Game.Deck (Card)

data Player = P1 | P2 deriving (Eq, Show)

data RoundResult = RoundWonBy Player | Tie deriving (Eq)

data HandResult = HandWonBy Player | NotFinished deriving (Eq)

data HandState = HandState
    { cardsPlayed    :: [(Player, Card)] -- which cards were played by which player
    , roundResults   :: [RoundResult]
    , startedBy      :: Player
    }

analyzeRounds :: Player -> [RoundResult] -> HandResult
analyzeRounds starter rs
    | length rs <= 1                                       = NotFinished
    | wonByPlayer P1 rs >= 2                               = HandWonBy P1
    | wonByPlayer P2 rs >= 2                               = HandWonBy P2
    | length rs == 2 && head rs /= Tie && rs !! 1 /= Tie   = NotFinished
    | head rs /= Tie && (rs !! 1 == Tie || rs !! 2 == Tie) = HandWonBy (winnerOfRound (head rs)) -- "Primera vale doble"
    | head rs == Tie && rs !! 1 /= Tie                     = HandWonBy (winnerOfRound (rs !! 1)) -- "Como parda la mejor"
    | head rs == Tie && rs !! 1 == Tie && rs !! 2 /= Tie   = HandWonBy (winnerOfRound (rs !! 2)) 
    | head rs == Tie && rs !! 1 == Tie && rs !! 2 == Tie   = HandWonBy starter                   -- "Gana la mano"
    | otherwise                                            = NotFinished
  where
    winnerOfRound (RoundWonBy p) = p
    winnerOfRound Tie = error "Unexpected Tie"
    wonByPlayer p rs' = length $ filter (== RoundWonBy p) rs'
