module Main where

import Game.Connection
import Game.Types
import Game.CLI
import Game.Core            (playGame)
import Control.Exception    (try)

import Network.Socket

initialGameState :: Player -> GameState
initialGameState p = GS
    { points         = (0, 0)
    , numberOfHands  = 1
    , toStart        = p
    }

agent :: Socket -> GameAgent
agent sock = GameAgent
    { initializeHand = initializeViaSocket sock
    , getAction      = getActionViaSocket sock
    }


main :: IO ()
main = do
    result <- try $ menu awaitForPlayer connectToPlayer
    case result of
        Left e                -> handleException e
        Right (sock, starter) -> playGame (agent sock) (initialGameState starter)
    
