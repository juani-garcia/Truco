module Main where

import Game.Connection
import Game.Types
import Game.CLI
import Game.Core         (playGame)

import Network.Socket

initialGameState :: Socket -> Player -> GameState
initialGameState sock p = GS
    { points         = (0, 0)
    , numberOfHands  = 1
    , toStart        = p
    , initializeHand = initializeViaSocket sock
    , getAction      = getActionViaSocket sock
    }


main :: IO ()
main = do
    (sock, starter) <- menu awaitForPlayer connectToPlayer
    playGame $ initialGameState sock starter
