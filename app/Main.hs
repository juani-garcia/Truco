module Main where

import Game.Deck
import Game.Core
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-- Initialize a new HandState
initializeHandState :: Player -> HandState
initializeHandState starter = HandState { cardsPlayed = [], roundResults = [], startedBy = starter }

-- A function to let a player choose a card from their hand
toList :: CardHand -> [Card]
toList (c1, c2, c3) = [c1, c2, c3]

chooseCard :: Player -> CardHand -> [(Player, Card)] -> IO Card
chooseCard player hand played = do
    -- Display the player's current hand
    putStrLn $ show player ++ ", your hand is: " ++ showHand hand
    putStrLn "Enter the number of the card to play (1, 2, or 3):"
    hFlush stdout
    input <- getLine
    let selected = inputToSelection input
    case selected of
        Nothing -> do
            putStrLn "Invalid input. Please try again."
            chooseCard player hand played
        Just c  -> do
            if wasPlayed c played
                then do
                    putStrLn "This card has already been played. Please try again."
                    chooseCard player hand played
                else return c
    where
    fst3 (x, _, _) = x
    snd3 (_, y, _) = y
    trd3 (_, _, z) = z
    inputToSelection input = case input of
        "1" -> Just $ fst3 hand
        "2" -> Just $ snd3 hand
        "3" -> Just $ trd3 hand
        _   -> Nothing

cardsPlayedToString :: [(Player, Card)] -> String
cardsPlayedToString [] = ""
cardsPlayedToString ((p, c):xs) = printf "Player %s played: %s\n" (show p) (show c) ++ cardsPlayedToString xs

wasPlayed :: Card -> [(Player, Card)] -> Bool
wasPlayed card played = card `elem` map snd played

-- Function to play a single round of the game
playRound :: Player -> HandState -> CardHand -> CardHand -> IO (HandState, Player)
playRound starter state hand1 hand2 = do
    -- Clear the terminal
    clearTerminal
    putStrLn $ printf "--- Round %d ---" (length (roundResults state) + 1)

    let played = cardsPlayed state
    putStr $ cardsPlayedToString played
    -- Player 1 selects a card
    card1 <- chooseCard P1 hand1 played
    -- Player 2 selects a card
    card2 <- chooseCard P2 hand2 played

    -- Display the cards played by both players
    putStrLn $ "Player 1 played: " ++ show card1
    putStrLn $ "Player 2 played: " ++ show card2

    -- Determine the round winner
    let roundResult = case compare card1 card2 of
            GT -> RoundWonBy P1
            LT -> RoundWonBy P2
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
            cardsPlayed = cardsPlayed state ++ [(P1, card1), (P2, card2)],
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
