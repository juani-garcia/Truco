module Main where

import Game.Deck
import Game.Core
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Control.Monad (when)
import Data.List (intercalate)

-- Initialize a new HandState
initializeHandState :: Player -> HandState
initializeHandState starter = HandState { cardsPlayed = [], roundResults = [], startedBy = starter }

-- A function to let a player choose a card from their hand
toList :: CardHand -> [Card]
toList (c1, c2, c3) = [c1, c2, c3]

chooseCard :: Player -> CardHand -> [(Player, Card)] -> IO Card
chooseCard player hand played = do
    let available = filter (\c -> not $ wasPlayed c played) (toList hand)
    when (null available) $ error "No available cards to play"
    putStrLn $ printf "%s - your hand is: %s" (show player) $ intercalate ", " (map show available)
    putStrLn $ printf "Enter the number of the card to play:"
    putStrLn $ formatAvailableCards available
    hFlush stdout
    input <- getLine
    let selected = inputToSelection input available
    case selected of
        Nothing -> do
            putStrLn "Invalid input. Please try again."
            chooseCard player hand played
        Just c  -> do
            return c
    where
        formatCard :: Int -> Card -> String
        formatCard n card = printf "  %d- %s" n (show card)

        formatAvailableCards :: [Card] -> String
        formatAvailableCards cards = intercalate "\n" $ zipWith formatCard [1..] cards
        
        inputToSelection input available =
            let n = read input
             in if n <= 0 || n > length available + 1
                then Nothing
                else Just (available !! (n - 1))

cardPlayedToString :: (Player, Card) -> String
cardPlayedToString (p, c) = printf "Player %s played: %s" (show p) (show c)

formatRoundResult :: RoundResult -> Int -> String
formatRoundResult Tie            = printf "Round %d was tied!"
formatRoundResult (RoundWonBy p) = printf "Player %s won round %d!" (show p)

wasPlayed :: Card -> [(Player, Card)] -> Bool
wasPlayed card played = card `elem` map snd played

-- Function to play a single round of the game
playRound :: Player -> HandState -> CardHand -> CardHand -> IO (HandState, Player)
playRound starter state hand1 hand2 = do
    -- Clear the terminal
    clearTerminal
    let n = length (roundResults state) + 1
    putStrLn $ printf "--- Round %d ---" n

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

    -- Announce the round result
    case roundResult of
        RoundWonBy P1 -> putStrLn "Player 1 wins the round!"
        RoundWonBy P2 -> putStrLn "Player 2 wins the round!"
        Tie           -> putStrLn "It's a tie!"

    -- Determine the next starter
    let nextStarter = case roundResult of
            RoundWonBy p -> p
            Tie          -> starter

    -- Update and return the new HandState
    let newState = state {
            cardsPlayed = cardsPlayed state ++ [(first, fstCard), (second, sndCard)],
            roundResults = roundResults state ++ [roundResult]
        }
    return (newState, nextStarter)
    where
        clearTerminal = do
            putStr "\ESC[2J"
            putStr "\ESC[H"

-- Function to manage the overall game flow
playGame :: IO ()
playGame = do
    -- Deal hands to both players (each hand contains three cards)
    hands <- deal 2
    let (hand1, hand2) = (head hands, hands !! 1)

    putStrLn "--- Welcome to the Card Game! ---"

    -- Start the game with an initial state
    let playRounds :: HandState -> Player -> CardHand -> CardHand -> IO ()
        playRounds state starter h1 h2 = do
            if length (roundResults state) == 3 || analyzeRounds (startedBy state) (roundResults state) /= NotFinished
                then do
                    -- Game ends after three rounds or when a player wins
                    putStrLn "--- Game Over ---"
                    case analyzeRounds (startedBy state) (roundResults state) of
                        HandWonBy P1 -> putStrLn "Player 1 wins the game!"
                        HandWonBy P2 -> putStrLn "Player 2 wins the game!"
                        _            -> error "Unexpected game result"
                else do
                    -- Play a single round
                    (newState, nextStarter) <- playRound starter state h1 h2
                    -- Continue to the next round
                    playRounds newState nextStarter h1 h2

    playRounds (initializeHandState P1) P1 hand1 hand2

-- Main function to initiate the game
main :: IO ()
main = playGame
