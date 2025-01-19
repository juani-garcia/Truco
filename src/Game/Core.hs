module Game.Core (playHand) where

import Game.Types
import Game.Mechanics
import Text.Printf      (printf)
import Game.Utils
import Control.Monad    (forM_, unless)
import Text.Read (readMaybe)
import Game.Deck
import Data.List

initialHandState :: Player -> CardHand -> CardHand -> HandState
initialHandState p h1 h2 = HS
    { hands        = (h1, h2)
    , actions      = []
    , cardsPlayed  = []
    , roundResults = []
    , currentRound = []
    , bettingState = NoBetting
    , startedBy    = p
    , currentPlayer   = p
    }

chooseAction :: Player -> HandState -> IO Action
chooseAction p s = do
    putStrLn $ printf "%s, es tu turno." (show p)
    let acs = availableCards
        pas = optsToActions (possibleActions s) acs
        len = length pas
    putStrLn $ printf "Tu mano es: %s." (intercalate ", " $ map show acs)
    unless (len > 0) $ error "No hay acciones válidas para realizar."
    forM_ (zip ([1..] :: [Int]) pas) $ \(i, a) -> do
        putStrLn $ printf "  %d. %s" i (show a)
    putStrLn "Elegí una de las opciones:"
    input <- getInput len
    unless (inBound len input) $ error "Entrada inválida."
    return $ pas !! (input - 1)
    where
        hand = case currentPlayer s of
            P1 -> fst (hands s)
            P2 -> snd (hands s) 
        availableCards = filter (not . (`elem` map snd (cardsPlayed s))) $ toCardList hand
        inBound l i = i >= 1 && i <= l
        getInput :: Int -> IO Int
        getInput l = do
            input <- getLine
            case readMaybe input of
                Just n | inBound l n -> return n
                _                          -> do
                    putStrLn $ printf "Entrada inválida. Por favor, ingrese un número entre 1 y %d." l
                    getInput l

handLoop :: HandState -> IO ()
handLoop s = do
    printHandState s
    action <- chooseAction curr s
    let ms = applyAction s action
    case ms of
        Nothing -> do
            putStrLn "Acción inválida. Por favor, intente nuevamente."
            handLoop s
        Just s' -> do
            let result = analyzeHands s'
            case result of
                NotFinished -> handLoop s'
                HandWonBy p -> putStrLn $ printf "¡El jugador %s ganó la mano!" (show p)
    where
        curr = currentPlayer s

playHand :: IO ()
playHand = do
    [h1, h2] <- deal 2 
    handLoop $ initialHandState P1 h1 h2
