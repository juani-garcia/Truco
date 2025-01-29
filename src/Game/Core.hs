module Game.Core where

import Game.Hand
import Game.Mechanics   (getWinner)
import Game.Types
import Text.Printf      (printf)
import Game.Utils (theOther)

gameLoop :: GameState -> IO ()
gameLoop gs = do
    finalState <- initializeHand gs gs >>= playHand
    let r@(p1, p2) = case handResult finalState of
            Just pair -> pair
            Nothing   -> error "La finalizó sin puntos asignados."
        gs'      = updateGameState gs r
        winner   = getWinner $ points gs'
    case winner of
        Just p  -> putStrLn $ printf "¡%s la partida!" $ if p == P1 then "Ganaste" else "Perdiste"
        Nothing -> do
            putStrLn "\n¡Finalizó la mano!"
            putStrLn $ "  Puntos para P1: " ++ show p1
            putStrLn $ "  Puntos para P2: " ++ show p2
            putStrLn   "\nPulse ENTER para continuar el juego..."
            _ <- getLine
            gameLoop gs'
    where
        updateGameState :: GameState -> PlayerPoints -> GameState
        updateGameState s@GS{ points = (p1, p2), numberOfHands = k } (p1', p2') = s
            { points        = (p1 + p1', p2 + p2')
            , numberOfHands = k + 1
            , toStart       = theOther $ toStart gs
            }

playGame :: GameState -> IO ()
playGame = gameLoop
