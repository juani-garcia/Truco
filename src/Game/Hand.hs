module Game.Hand (playHand) where

import Game.Types
import Game.Mechanics
import Game.Utils
import Text.Printf              (printf)
import Control.Monad            (forM_, when, unless)
import Data.List.Extra          (upper, lower)
import System.Process.Extra     (system)

printHandState :: HandState -> IO ()
printHandState hs = do
    _ <- clear
    printGameState $ gameState hs
    let rs = roundResults hs
        as = actions hs
        n  = length rs + 1
    putStrLn $ printf "--- %s ---" (upper $ roundName n)
    unless (null rs) $ putStrLn "Resultados de cada ronda:"
    forM_ (zip [1..] rs) $ \(i, r) -> do 
        case r of
            RoundWonBy p -> putStrLn $ printf "  %s ganó %s." (show p) (lower $ roundName i)
            Tie          -> putStrLn $ printf "  %s terminó empatada." (lower $ roundName i)

    unless (null as) $ putStrLn "Acciones de esta mano:"
    forM_ as $ \(p, a) -> 
        putStrLn $ printf "  %s: %s." (show p) (showPastActions a)
    
    when (showEnvido hs) $ printEnvido hs

    where
        roundName :: Int -> String
        roundName k = case k of
            1 -> "Primera"
            2 -> "Segunda"
            3 -> "Tercera"
            _ -> ""
        clear = system "clear"

printEnvido :: HandState -> IO ()
printEnvido hs = do
    putStrLn "Resultados del envido:"
    putStrLn $ printf "  %s tiene %d" (show p1) e1
    if e2 > e1
        then do
            putStrLn $ printf "  %s dice que %d son mejores." (show p2) e2
        else do
            putStrLn $ printf "  %s dice que %d son buenas..." (show p2) e1
    where
        p1 = startedBy hs
        p2 = theOther p1
        e1 = envido $ getPlayerInfo p1 (hands hs)
        e2 = envido $ getPlayerInfo p2 (hands hs)

printGameState :: GameState -> IO () -- Para proveer un poco de contexto acerca de la partida
printGameState gs = do
    putStrLn "INFORMACIÓN DE LA PARTIDA"
    let (p1, p2) = points gs
        n        = numberOfHands  gs
    putStrLn $ printf "     Mano #%d -> P1 %d\n                P2 %d" n p1 p2

handLoop :: HandState -> IO HandState
handLoop hs = do
    printHandState hs
    action <- getAction (gameState hs) hs
    let ms = applyAction hs action
    case ms of
        Nothing -> do
            putStrLn "Acción inválida. Por favor, intente nuevamente."
            handLoop hs
        Just hs' -> do
            let result = analyzeHand hs'
            case result of
                TrucoNotFinished -> handLoop hs'
                _           -> do
                    return hs'{ handResult = Just (getHandPoints P1 hs', getHandPoints P2 hs') }

playHand :: HandState -> IO HandState
playHand = handLoop
