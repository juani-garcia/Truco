module Game.CLI where

import Game.Types
import Game.Utils
import Game.Mechanics

import Control.Monad        (forM_, when, unless)
import Text.Printf          (printf)
import Network.Socket       (Socket)
import Data.List            (intercalate, partition)
import Data.Bifunctor       (bimap)
import Text.Read            (readMaybe)
import System.Process.Extra (system)

menu :: IO (Socket, GameState) -> IO (Socket, GameState) -> IO (Socket, GameState)
menu awaitFor connectTo = do
    putStrLn "Seleccione una opción:"
    putStrLn "1. Esperar por jugador"
    putStrLn "2. Conectarse a jugador"
    option <- getLine
    readOption option
  where
    readOption opt = case opt of
        "1" -> awaitFor
        "2" -> connectTo
        _   -> do
            putStrLn "Opción inválida, por favor intente de nuevo."
            menu awaitFor connectTo

printHandState :: GameState -> HandState -> IO ()
printHandState gs hs = do
    clear
    printGameState
    printRoundInfo
    printActions
    printEnvido
  where
    clear = do
        _ <- system "clear"
        return ()

    roundName :: Int -> String
    roundName k = case k of
        1 -> "Primera"
        2 -> "Segunda"
        3 -> "Tercera"
        _ -> ""

    nameP1 = getPlayerInfo P1 $ names gs
    nameP2 = getPlayerInfo P2 $ names gs

    printGameState = do
        putStrLn "INFORMACIÓN DE LA PARTIDA"
        let (p1, p2) = points gs
            n        = numberOfHands gs
        putStrLn $ printf "Mano #%-2d -> %-16s %d" n nameP1 p1
        putStrLn $ printf "            %-16s %d" nameP2 p2

    printRoundInfo = do
        let header = printf "| %-10s | %-16s | %-16s |" "Ronda" nameP1 nameP2
        putStrLn $ replicate (length header) '-'
        putStrLn header
        putStrLn $ "|" ++ replicate (length header - 2) '-' ++ "|"
        forM_ (zip3 [1..3] cardsByPlayer maybeResults) $ \(i, mcsi, mri) -> do
            case mcsi of
                [mc1i, mc2i] -> putStrLn $ formatRound i mc1i mc2i mri
                _            -> error "Mano inválida"
        putStrLn $ replicate (length header) '-'
      where
        formatRound :: Int -> Maybe Card -> Maybe Card -> Maybe RoundResult -> String
        formatRound i mc1i mc2i mri = printf "| %-10s | %-16s | %-16s |" (roundName i) (formatCard mc1i) (formatCard mc2i) ++ maybe "" formatResult mri
          where
            formatCard :: Maybe Card -> String
            formatCard = maybe "" show

            formatResult :: RoundResult -> String
            formatResult r = case r of
                RoundWonBy p -> " ---> Ganó " ++ getPlayerInfo p (names gs)
                Tie          -> " ---> Empate"


        cardsByPlayer = let (cs1 , cs2) = bimap aux aux $ partition ((==P1) . fst) $ cardsPlayed hs
                        in zipWith (\x y -> [x, y]) cs1 cs2
          where
            aux cs = take 3 $ map (Just . snd) cs ++ repeat Nothing

        maybeResults = take 3 $ map Just (roundResults hs) ++ repeat Nothing

    printActions = do
        let pas = filter (simpleAction . snd) $ actions hs
        unless (null pas) $ putStrLn "Acciones de la mano:"
        forM_ pas $ \(p, a) -> do
            putStrLn $ printf "  %s: %s" (getPlayerInfo p (names gs)) (showPastActions a)

      where
        simpleAction (PlayCard _) = False
        simpleAction _            = True  

    printEnvido = when (showEnvido hs) $ do
        putStrLn "Resultados del envido:"
        putStrLn $ printf "  %s tiene %d" n1 e1
        if e2 > e1
            then do
                putStrLn $ printf "  %s dice que %d son mejores." n2 e2
            else do
                putStrLn $ printf "  %s dice que %d son buenas..." n2 e1
      where
        p1 = startedBy hs
        p2 = theOther p1
        n1 = getPlayerInfo p1 $ names gs
        n2 = getPlayerInfo p2 $ names gs
        e1 = envido $ getPlayerInfo p1 (hands hs)
        e2 = envido $ getPlayerInfo p2 (hands hs)


printHandResult :: GameState -> Maybe Player -> IO ()
printHandResult gs@GS{ points = (p1, p2) } winner = do
    case winner of
        Just p  -> putStrLn $ printf "¡%s la partida!" $ if p == P1 then "Ganaste" else "Perdiste"
        Nothing -> do
            putStrLn "\n¡Finalizó la mano!"
            putStrLn $ printf "  Puntos para %s: %d" (name P1) p1
            putStrLn $ printf "  Puntos para %s: %d" (name P2) p2
            putStrLn   "\nPulse ENTER para continuar el juego..."
            _ <- getLine
            return ()
  where
    name p = getPlayerInfo p $ names gs

getActionLocally :: HandState -> IO Action
getActionLocally s = do
    putStrLn "¡Es tu turno!"
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
    hand = getPlayerInfo (currentPlayer s) (hands s)
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


