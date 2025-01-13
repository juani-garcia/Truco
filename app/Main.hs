module Main where

import Game.Mechanics (envido)
import Game.Deck (createDeck, getRandomHand, showHand)

main :: IO ()
main = do
    let deck = createDeck
    hand <- getRandomHand deck
    putStrLn $ "Mano:        " ++ showHand hand
    putStrLn $ "Envido:      " ++ show (envido hand)
    putStrLn $ "Mayor carta: " ++ show (maximum $ (\(a, b, c) -> [a, b, c]) hand)

