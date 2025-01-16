module Game.Deck where

import Game.Types
import Utils

createDeck :: Deck
createDeck =
    [ Card { getNumber = n, getSuit = s }
    | n <- [1..7] ++ [10..12]  -- 1 to 7, and 10 (Sota), 11 (Caballo), 12 (Rey)
    , s <- [Oro, Copa, Espada, Basto]
    ]

deal :: Int -> IO [CardHand]
deal n = do
    shuffledDeck <- shuffle createDeck
    return $ take n $ dealHands shuffledDeck
    where
        dealHands :: Deck -> [CardHand]
        dealHands [] = []
        dealHands (c1:c2:c3:cs) = (c1, c2, c3) : dealHands cs
        dealHands _ = error "No hay cartas suficientes para repartir"
