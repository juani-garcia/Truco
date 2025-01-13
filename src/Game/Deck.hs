module Game.Deck 
    ( Suit(..)
    , Card(..)
    , Deck
    , createDeck
    , Hand
    , showHand
    , getRandomHand
    ) where

import System.Random (randomRIO)

data Suit = Oro | Copa | Espada | Basto deriving (Show, Eq)
data Card = Card
    { getNumber :: Int
    , getSuit   :: Suit
    }

cardValue :: Card -> Int
cardValue card = case (getNumber card, getSuit card) of
    (1, Espada) -> 14 
    (1, Basto)  -> 13
    (7, Espada) -> 12
    (7, Oro)    -> 11
    (3, _)      -> 10
    (2, _)      -> 9
    (1, _)      -> 8
    (12, _)     -> 7
    (11, _)     -> 6   
    (10, _)     -> 5
    (7, _)      -> 4
    (6, _)      -> 3
    (5, _)      -> 2
    (4, _)      -> 1
    _           -> 0   -- Should not happen.

instance Eq Card where
    c1 == c2 = cardValue c1 == cardValue c2

instance Ord Card where
    compare c1 c2 = compare (cardValue c1) (cardValue c2)

instance Show Card where
    show card = show (getNumber card) ++ " de " ++ show (getSuit card)

type Deck = [Card]

createDeck :: Deck
createDeck =
    [ Card { getNumber = n, getSuit = s }
    | n <- [1..7] ++ [10..12]  -- 1 to 7, and 10 (Sota), 11 (Caballo), 12 (Rey)
    , s <- [Oro, Copa, Espada, Basto]
    ]   

getRandomHand :: Deck -> IO Hand
getRandomHand deck = do
    let deckSize = length deck
    idx1 <- randomRIO (0, deckSize - 1)
    idx2 <- randomRIO (0, deckSize - 1) `suchThat` (/= idx1)
    idx3 <- randomRIO (0, deckSize - 1) `suchThat` (\x -> x /= idx1 && x /= idx2)
    let card1 = deck !! idx1
    let card2 = deck !! idx2
    let card3 = deck !! idx3
    return (card1, card2, card3)

suchThat :: IO Int -> (Int -> Bool) -> IO Int
suchThat gen predicate = do
    value <- gen
    if predicate value
        then return value
        else suchThat gen predicate

type Hand = (Card, Card, Card)

showHand :: Hand -> String
showHand (c1, c2, c3) = show c1 ++ ", " ++ show c2 ++ ", " ++ show c3
