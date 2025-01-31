module Game.Core (playGame) where

import Game.Hand
import Game.Types
import Game.Utils                       (theOther)
import Game.CLI                         (printHandResult)
import Game.Mechanics                   (getWinner, getHandResult)

import Data.Maybe                       (isNothing)
import Control.Monad                    (when)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.State.Lazy   (StateT, get, modify, evalStateT)

type GameMonad = StateT GameState IO

playGame :: GameState -> IO ()
playGame = evalStateT loop

loop :: GameMonad ()
loop = do
    gs  <- get
    hs  <- liftIO $ initializeHand gs gs >>= playHand
    let res = getHandResult hs
    modify $ updateGameState res
    gs' <- get
    let winner = getWinner $ points gs'
    liftIO $ printHandResult res winner
    when (isNothing winner) loop
  where
    updateGameState :: PlayerPoints -> GameState -> GameState
    updateGameState (p1', p2') s@GS{ points = (p1, p2), numberOfHands = k } = s
        { points        = (p1 + p1', p2 + p2')
        , numberOfHands = k + 1
        , toStart       = theOther $ toStart s
        }
