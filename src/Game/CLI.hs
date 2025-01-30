module Game.CLI where

import Game.Types
import Game.Utils
import Game.Mechanics 

import Control.Monad        (forM_, when, unless)
import Text.Printf          (printf)
import Data.List.Extra      (lower, upper)
import Network.Socket       (Socket)
import Data.List            (intercalate)
import Text.Read            (readMaybe)

menu :: IO Socket -> IO Socket -> IO (Socket, Player)
menu awaitFor connectTo = do
    putStrLn "Seleccione una opción:"
    putStrLn "1. Esperar por jugador"
    putStrLn "2. Conectarse a jugador"
    option <- getLine
    case option of
        "1" -> do
            s <- awaitFor
            return (s, P1)
        "2" -> do
            s <- connectTo
            return (s, P2)
        _   -> do
            putStrLn "Opción inválida, por favor intente de nuevo."
            menu awaitFor connectTo

printHandState :: HandState -> IO ()
printHandState hs = do
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
        n        = numberOfHands gs
    putStrLn $ printf "     Mano #%d -> P1 %d\n                P2 %d" n p1 p2

printHandResult :: PlayerPoints -> Maybe Player -> IO ()
printHandResult (p1, p2) winner = do    
    case winner of
        Just p  -> putStrLn $ printf "¡%s la partida!" $ if p == P1 then "Ganaste" else "Perdiste"
        Nothing -> do
            putStrLn "\n¡Finalizó la mano!"
            putStrLn $ "  Puntos para P1: " ++ show p1
            putStrLn $ "  Puntos para P2: " ++ show p2
            putStrLn   "\nPulse ENTER para continuar el juego..."
            _ <- getLine
            return ()

getActionLocally :: HandState -> IO Action
getActionLocally s = do
    putStrLn $ printf "%s, es tu turno." (show p)
    let acs = availableCards
        pas = optsToActions (possibleActions s) acs
        len = length pas
    putStrLn $ printf "Tu mano es: %s." (intercalate ", " $ map show acs)
    unless (len > 0) $ error "No hay acciones válidas para realizar."
    forM_ (zip ([1..] :: [Int]) pas) $ \(i, a) -> putStrLn $ printf "  %d. %s" i (show a)
    putStrLn "Elegí una de las opciones:"
    input <- getInput len
    unless (inBound len input) $ error "Entrada inválida."
    return $ pas !! (input - 1)
  where
        p = currentPlayer s
        hand = case currentPlayer s of
            P1 -> fst (hands s)
            P2 -> snd (hands s)
        availableCards = filter (`notElem` map snd (cardsPlayed s)) $ toCardList hand
        inBound l i = i >= 1 && i <= l
        getInput :: Int -> IO Int
        getInput l = do
            input <- getLine
            case readMaybe input of
                Just n | inBound l n -> return n
                _                          -> do
                    putStrLn $ printf "Entrada inválida. Por favor, ingrese un número entre 1 y %d." l
                    getInput l


