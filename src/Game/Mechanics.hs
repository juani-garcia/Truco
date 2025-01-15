module Game.Mechanics where

import Game.Deck

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

-- Round mechanics
-- data State = 
--       Initial
--     | EnvidoCalled Int
--     | EnvidoEnded -- Either called or not
--     | TrucoCalled Int
--     | Truco Int
--     | Finished
--     deriving (Show, Eq)

-- data Action =
--       Play
--     | CallEnvido
--     | CallTruco
--     | Accept
--     | Reject
--     | Fold

-- possibleActions :: State -> [Action]
-- possibleActions Initial          = [Play, CallEnvido, CallTruco, Fold]
-- possibleActions (EnvidoCalled _) = [Accept, Reject]
-- possibleActions EnvidoEnded      = [Play, CallTruco, Fold]
-- possibleActions (TrucoCalled _)  = [Accept, Reject]
-- possibleActions (Truco _)        = [Play, Fold]
-- possibleActions Finished         = []

-- transition :: State -> Action -> Maybe State
-- transition Initial          Play       = Just Initial
-- transition Initial          CallEnvido = Just (EnvidoCalled 2)
-- transition Initial          CallTruco  = Just (TrucoCalled 2)
-- transition (EnvidoCalled _) _          = Just EnvidoEnded
-- transition EnvidoEnded      Play       = Just EnvidoEnded
-- transition EnvidoEnded      CallTruco  = Just (TrucoCalled 2)
-- transition (TrucoCalled _)  Accept     = Just (Truco 2)
-- transition (TrucoCalled _)  Reject     = Just Finished
-- transition (Truco n)        Play       = Just (Truco n)
-- transition _                Fold       = Just Finished
-- transition _                _          = Nothing

-- data Player = Player1 | Player2 deriving (Show, Eq)

-- type Points = (Int, Int)

-- addPoints :: Points -> Player -> Int -> Points
-- addPoints (p1, p2) Player1 n = (p1 + n, p2)
-- addPoints (p1, p2) Player2 n = (p1, p2 + n)

