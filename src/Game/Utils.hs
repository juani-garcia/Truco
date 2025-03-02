module Game.Utils where

import Data.Array.IO
import System.Random    (randomRIO)
import Game.Types
import Control.Monad    (join, forM)

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
        
chooseRandomIndices :: IO [Int] -- Me devuelve los índices de las cartas que se van a usar para una mano
chooseRandomIndices = take 6 <$> shuffle [0..39]

theOther :: Player -> Player
theOther P1 = P2
theOther P2 = P1

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

getPlayerInfo :: Player -> (a, a) -> a
getPlayerInfo p (x1, x2) = if p == P1 then x1 else x2

initialState :: CardHand -> CardHand -> Player -> HandState
initialState h1 h2 p = HS
    { hands         = (h1, h2)
    , actions       = []
    , cardsPlayed   = []
    , roundResults  = []
    , currentRound  = []
    , bettingState  = NoBetting
    , startedBy     = p
    , currentPlayer = p
    , trucoPoints   = 1
    , envidoPoints  = 0
    , envidoWonBy   = Nothing
    , showEnvido    = False
    }
