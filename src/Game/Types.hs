module Game.Types where

-- Type definitions related to cards and deck
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

type CardHand = (Card, Card, Card)

-- Type definitions related to players and game state
data Player = P1 | P2 deriving (Eq, Show)

data RoundResult = RoundWonBy Player | Tie deriving (Eq)

data HandResult = HandWonBy Player | NotFinished deriving (Eq)

data HandState = HandState
    { cardsPlayed    :: [(Player, Card)] -- which cards were played by which player
    , roundResults   :: [RoundResult]
    , startedBy      :: Player
    }
