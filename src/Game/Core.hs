module Game.Core (playGame) where

import Game.Hand
import Game.Types
import Game.Utils                       (theOther)
import Game.CLI                         (printHandResult)
import Game.Mechanics                   (getWinner, getHandResult)

import Data.Maybe                       (isNothing)
import Control.Monad                    (when)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Extra              (void)
import Control.Monad.Trans.RWS.Strict   (RWST, get, gets, modify, ask, evalRWST)

type GameMonad = RWST GameAgent () GameState IO

playGame :: GameAgent -> Player -> IO ()
playGame agent p = void $ evalRWST gameLoop agent initialGameState
  where
    initialGameState = GS
        { points         = (0, 0)
        , numberOfHands  = 1
        , toStart        = p
        }

gameLoop :: GameMonad ()
gameLoop = do
    agent <- ask
    gs  <- get
    hs  <- liftIO $ initializeHand agent gs >>= playHand (agent, gs)
    let res = getHandResult hs
    modify $ updateGameState res
    winner <- gets (getWinner . points)
    liftIO $ printHandResult res winner
    when (isNothing winner) gameLoop
  where
    updateGameState :: PlayerPoints -> GameState -> GameState
    updateGameState (p1', p2') s@GS{ points = (p1, p2), numberOfHands = k } = s
        { points        = (p1 + p1', p2 + p2')
        , numberOfHands = k + 1
        , toStart       = theOther $ toStart s
        }
