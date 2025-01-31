module Game.Deck where

import Game.Types

createDeck :: Deck
createDeck =
    [ Card { getNumber = n, getSuit = s }
    | n <- [1..7] ++ [10..12]  -- 1 to 7, and 10 (Sota), 11 (Caballo), 12 (Rey)
    , s <- [Oro, Copa, Espada, Basto]
    ]

deal :: [Int] -> (CardHand, CardHand)
deal indices = do
    let selectedCards = map (createDeck !!) indices
    case selectedCards of
        [c1, c2, c3, c4, c5, c6] -> ((c1, c2, c3), (c4, c5, c6))
        _ -> error "Se necesitan exactamente 6 índices para repartir las manos"
