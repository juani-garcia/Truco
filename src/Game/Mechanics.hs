module Game.Mechanics where

import Game.Types

-- Envido calculations
suited :: Card -> Card -> Bool
suited c1 c2 = getSuit c1 == getSuit c2

cardEnvidoValue :: Card -> Int
cardEnvidoValue c = if n >= 10 then 0 else n
    where n = getNumber c

suitedEnvidoConstant :: Int
suitedEnvidoConstant = 20

pairwiseEnvido :: Card -> Card -> Int
pairwiseEnvido c1 c2 = if suited c1 c2
    then suitedEnvidoConstant + cardEnvidoValue c1 + cardEnvidoValue c2
    else cardEnvidoValue c1 `max` cardEnvidoValue c2

envido :: CardHand -> Int
envido (c1, c2, c3) = maximum 
    [ pairwiseEnvido c1 c2
    , pairwiseEnvido c1 c3
    , pairwiseEnvido c2 c3
    ]

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

