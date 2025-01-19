module Game.Utils where

import Data.Array.IO
import System.Random            (randomRIO)
import Control.Monad            (forM, forM_, join, unless)
import Game.Types
import Text.Printf              (printf)
import Data.List.Extra          (upper, lower)
import System.Process.Extra     (system)

-- TODO: esto es una bolsa de gatos, emprolijar
shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- ioArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
    where
        n = length xs
        ioArray :: Int -> [a] -> IO (IOArray Int a)
        ioArray l = newListArray (1,l)
        
theOther :: Player -> Player
theOther P1 = P2
theOther P2 = P1

suited :: Card -> Card -> Bool
suited c1 c2 = getSuit c1 == getSuit c2

cardEnvidoValue :: Card -> Int
cardEnvidoValue c = if n >= 10 then 0 else n
    where n = getNumber c

suitedEnvidoConstant :: Int
suitedEnvidoConstant = 20

toCardList :: CardHand -> [Card]
toCardList (c1, c2, c3) = [c1, c2, c3]

optsToActions :: [ActionOpt] -> [Card] -> [Action]
optsToActions opts cs = join $ map (aux cs) opts
    where
        aux :: [Card] -> ActionOpt -> [Action]
        aux cs' (P f) = map f cs'
        aux _   (S a) = [a]

getResult :: [PlayerCard] -> Maybe RoundResult
getResult [(p1, c1), (p2, c2)] = Just $ case compare c1 c2 of
    GT -> RoundWonBy p1
    LT -> RoundWonBy p2
    EQ -> Tie
getResult _                    = Nothing

printHandState :: HandState -> IO ()
printHandState s = do
    let rs = roundResults s
        as = actions s
        n  = length rs + 1
    _ <- clear
    putStrLn $ printf "--- %s ---" (upper $ roundName n)
    unless (null rs) $ putStrLn "Resultados de cada ronda:"
    forM_ (zip [1..] rs) $ \(i, r) -> do 
        case r of
            RoundWonBy p -> putStrLn $ printf "  %s ganó %s." (show p) (lower $ roundName i)
            Tie          -> putStrLn $ printf "  %s terminó empatada." (lower $ roundName i)

    unless (null as) $ putStrLn "Acciones de esta mano:"
    forM_ as $ \(p, a) -> 
        putStrLn $ printf "  %s: %s." (show p) (showPastActions a)
    where
        roundName :: Int -> String
        roundName k = case k of
            1 -> "Primera"
            2 -> "Segunda"
            3 -> "Tercera"
            _ -> ""
        clear = system "clear"

showPastActions :: Action -> String
showPastActions (PlayCard c) = "jugó " ++ show c
showPastActions CallTruco    = "cantó truco"
showPastActions Accept       = "aceptó el truco"
showPastActions Decline      = "no quiso el truco"
showPastActions Fold         = "se fue al mazo"
