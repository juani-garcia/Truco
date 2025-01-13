module Game.Mechanics where

import Game.Deck

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

envido :: Hand -> Int
envido (c1, c2, c3) = maximum 
    [ pairwiseEnvido c1 c2
    , pairwiseEnvido c1 c3
    , pairwiseEnvido c2 c3
    ]
