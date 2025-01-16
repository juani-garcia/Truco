module Game.Core where

import Game.Deck
import Game.Types
import Game.Mechanics
import System.IO        (hFlush, stdout)
import Text.Printf      (printf)
import Text.Read        (readMaybe)
import Control.Monad    (when)
import Data.List        (intercalate)

initializeHandState :: Player -> HandState
initializeHandState starter = HandState { cardsPlayed = [], roundResults = [], startedBy = starter }

toList :: CardHand -> [Card]
toList (c1, c2, c3) = [c1, c2, c3]

chooseCard :: Player -> CardHand -> [(Player, Card)] -> IO Card
chooseCard player hand played = do
    let available = filter (\c -> not $ wasPlayed c played) (toList hand)
    when (null available) $ error "No hay cartas disponibles para jugar"
    putStrLn $ printf "%s, tu mano es: %s" (show player) $ intercalate ", " (map show available)
    putStrLn $ printf "Ingresa el número de la carta para jugarla:"
    putStrLn $ formatAvailableCards available
    hFlush stdout
    input <- getLine
    let selected = inputToSelection input available
    case selected of
        Nothing -> do
            putStrLn "Entrada inválida. Por favor, intente nuevamente."
            chooseCard player hand played
        Just c  -> do
            return c
    where
        formatCard :: Int -> Card -> String
        formatCard n card = printf "  %d- %s" n (show card)

        formatAvailableCards :: [Card] -> String
        formatAvailableCards cards = intercalate "\n" $ zipWith formatCard [1..] cards

        inputToSelection input available = do
            n <- readMaybe input
            if n <= 0 || n > length available
                then Nothing
                else Just (available !! (n - 1))

cardPlayedToString :: (Player, Card) -> String
cardPlayedToString (p, c) = printf "El jugador %s jugó: %s" (show p) (show c)

formatRoundResult :: RoundResult -> Int -> String
formatRoundResult Tie            = printf "¡La ronda %d terminó empatada!"
formatRoundResult (RoundWonBy p) = printf "¡El jugador %s ganó la ronda %d!" (show p)

wasPlayed :: Card -> [(Player, Card)] -> Bool
wasPlayed card played = card `elem` map snd played

playRound :: Player -> HandState -> CardHand -> CardHand -> IO (HandState, Player)
playRound starter state hand1 hand2 = do
    clearTerminal
    let n = length (roundResults state) + 1
    putStrLn $ printf "--- Ronda %d ---" n

    let played = cardsPlayed state
    mapM_ (putStrLn . cardPlayedToString) played
    mapM_ putStrLn $ zipWith formatRoundResult (roundResults state) [1..]

    let (first, firstHand, second, secondHand) = case starter of
            P1 -> (P1, hand1, P2, hand2)
            P2 -> (P2, hand2, P1, hand1)

    fstCard <- chooseCard first firstHand played
    putStrLn $ cardPlayedToString (first, fstCard)
    sndCard <- chooseCard second secondHand ((first, fstCard) : played)
    putStrLn $ cardPlayedToString (second, sndCard)

    let roundResult = case compare fstCard sndCard of
            GT -> RoundWonBy first
            LT -> RoundWonBy second
            EQ -> Tie

    putStrLn $ formatRoundResult roundResult n

    let nextStarter = case roundResult of
            RoundWonBy p -> p
            Tie          -> starter

    let newState = state {
            cardsPlayed = cardsPlayed state ++ [(first, fstCard), (second, sndCard)],
            roundResults = roundResults state ++ [roundResult]
        }
    return (newState, nextStarter)
    where
        clearTerminal = do
            putStr "\ESC[2J"
            putStr "\ESC[H"

playGame :: IO ()
playGame = do
    hands <- deal 2
    let (hand1, hand2) = (head hands, hands !! 1)
    playRounds (initializeHandState P1) P1 hand1 hand2
    where
        playRounds :: HandState -> Player -> CardHand -> CardHand -> IO ()
        playRounds state starter h1 h2 = do
            let results = roundResults state
            let handResult = analyzeRounds (startedBy state) results
            case handResult of
                HandWonBy p -> do
                    putStrLn "--- Terminó la mano ---"
                    putStrLn $ printf "¡El jugador %s ganó la mano!" (show p)
                NotFinished -> do
                    when (length results >= 3) $ error "La mano debería haber terminado"
                    (newState, nextStarter) <- playRound starter state h1 h2
                    playRounds newState nextStarter h1 h2

