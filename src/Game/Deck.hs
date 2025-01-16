module Game.Deck
    ( Suit(..)
    , Card(..)
    , Deck
    , createDeck
    , deal
    , CardHand
    ) where

import System.Random (randomRIO)
import Data.Array.IO
import Control.Monad

data Suit = Oro | Copa | Espada | Basto deriving (Show, Eq)
data Card = Card
    { getNumber :: Int
    , getSuit   :: Suit
    } deriving Eq

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

type CardHand = (Card, Card, Card)

shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- ioArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
    where
        n = length xs
        ioArray :: Int -> [a] -> IO (IOArray Int a)
        ioArray l = newListArray (1,l)

deal :: Int -> IO [CardHand]
deal n = do
    shuffledDeck <- shuffle createDeck
    return $ take n $ dealHands shuffledDeck
    where
        dealHands :: Deck -> [CardHand]
        dealHands [] = []
        dealHands (c1:c2:c3:cs) = (c1, c2, c3) : dealHands cs
        dealHands _ = error "Not enough cards to deal hands"
