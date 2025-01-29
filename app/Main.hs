module Main where

import Game.Connection
import Game.Types
import Network.Socket
import Game.Core (playGame)

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
    (sock, starter) <- menu
    playGame $ initialGameState sock starter
