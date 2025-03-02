module Game.Types where
import Network.Socket (HostName, ServiceName)

-- Type definitions related to cards and deck
data Suit = Oro | Copa | Espada | Basto deriving (Show, Eq)

data Card = Card
    { getNumber :: Int
    , getSuit   :: Suit
    } deriving Eq

cardValue :: Card -> Int
cardValue card = case (getNumber card, getSuit card) of
    (1,  Espada) -> 14
    (1,  Basto)  -> 13
    (7,  Espada) -> 12
    (7,  Oro)    -> 11
    (3,  _)      -> 10
    (2,  _)      -> 9
    (1,  _)      -> 8
    (12, _)      -> 7
    (11, _)      -> 6
    (10, _)      -> 5
    (7,  _)      -> 4
    (6,  _)      -> 3
    (5,  _)      -> 2
    (4,  _)      -> 1
    _            -> 0   -- Should not happen.

instance Ord Card where
    compare c1 c2 = compare (cardValue c1) (cardValue c2)

instance Show Card where
    show card = show (getNumber card) ++ " de " ++ show (getSuit card)

type Deck = [Card]

type CardHand = (Card, Card, Card)

-- Type definitions related to players and game state
data Player = P1 | P2 deriving (Eq, Show)

data RoundResult = RoundWonBy Player | Tie deriving (Eq, Show)

data TrucoResult = TrucoWonBy Player | TrucoNotFinished deriving (Eq, Show)

data Action
    = PlayCard Card
    | CallEnvido
    | CallRealEnvido
    | CallFaltaEnvido
    | CallTruco
    | CallReTruco
    | CallValeCuatro
    | Accept
    | Decline
    | Fold              -- "Me voy al mazo"
    deriving (Eq)

-- P por Parametrized, S por Simple
data ActionOpt = P (Card -> Action) | S Action

instance Show Action where
    show (PlayCard c)    = "Jugar " ++ show c
    show CallEnvido      = "Cantar envido"
    show CallRealEnvido  = "Cantar real envido"
    show CallFaltaEnvido = "Echar la falta"
    show CallTruco       = "Cantar truco"
    show CallReTruco     = "Cantar re truco"
    show CallValeCuatro  = "Cantar vale cuatro"
    show Accept          = "Aceptar"
    show Decline         = "Rechazar"
    show Fold            = "Irse al mazo"

data BettingState -- Estado de las "apuestas" de la mano. Si se cantó envido o truco, si se quiso, etc. 
    = NoBetting
    | EnvidoOffered Int
    | EnvidoAccepted Int
    | TrucoOffered Int
    | TrucoAccepted Int
    | HandEnded -- Se fue al mazo o no quiso el truco
    deriving (Eq, Show)

type PlayerCard   = (Player, Card)
type PlayerHand   = (Player, CardHand) 
type PlayerAction = (Player, Action)

data HandState = HS
    { hands         :: (CardHand, CardHand)
    , actions       :: [PlayerAction]      -- Qué hizo cada jugador
    , cardsPlayed   :: [PlayerCard]        -- Se puede derivar del campo anterior, pero más fácil tenerlo aparte
    , roundResults  :: [RoundResult]
    , currentRound  :: [PlayerCard]
    , bettingState  :: BettingState
    , startedBy     :: Player              -- Quién es "mano"
    , currentPlayer :: Player
    , trucoPoints   :: Int
    , envidoPoints  :: Int
    , envidoWonBy   :: Maybe Player
    , showEnvido    :: Bool
    }

type PlayerPoints = (Int, Int)
type Name = String
type PlayerNames = (Name, Name)

data GameState = GS
    { points         :: PlayerPoints
    , numberOfHands  :: Int
    , toStart        :: Player            -- El jugador que arranca la mano
    , names          :: PlayerNames
    }

data GameAgent = GameAgent
    { initializeHand :: GameState -> IO HandState
    , getAction      :: HandState -> IO Action
    }

data Options = Options
    { mhost       :: Maybe HostName
    , mport       :: Maybe ServiceName
    , mname       :: Maybe Name
    , listenFlag  :: Bool
    , connectFlag :: Bool
    } deriving Show
