module Game.Core where

import Game.Hand
import Game.Types
import Text.Printf      (printf)

initialGameState :: GameState
initialGameState = GS
    { points        = (0, 0)
    , numberOfHands = 1
    }

gameLoop :: GameState -> IO ()
gameLoop gs = do
    hs <- playHand gs
    let r@(p1, p2) = case handResult hs of
            Just pair -> pair
            Nothing   -> error "La mano no tiene puntos asignados"
        gs'      = updateGameState gs r
        winner   = getWinner gs'
    case winner of 
        Just p  -> putStrLn $ printf "¡%s ganó la partida!" (show p)
        Nothing -> do
            putStrLn "\n¡Finalizó la mano!"
            putStrLn $ "  Puntos para P1: " ++ show p1
            putStrLn $ "  Puntos para P2: " ++ show p2
            putStrLn   "\nPulse ENTER para continuar el juego..."
            _ <- getLine
            gameLoop gs' 
    where
        updateGameState :: GameState -> PlayerPoints -> GameState
        updateGameState GS{ points = (p1, p2), numberOfHands = k } (p1', p2') = GS 
            { points = (p1 + p1', p2 + p2')
            , numberOfHands = k + 1
            }
        
        pointsToWin :: Int
        pointsToWin = 30

        getWinner :: GameState -> Maybe Player
        getWinner GS{ points = (p1, p2) }
            | p1 >= pointsToWin  = Just P1
            | p2 >= pointsToWin  = Just P2
            | otherwise = Nothing

playGame :: IO ()
playGame = gameLoop initialGameState
