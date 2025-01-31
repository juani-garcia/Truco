module Game.Core where

import Game.Hand
import Game.Types
import Game.Utils                       (theOther)
import Game.CLI                         (printHandResult)
import Game.Mechanics                   (getWinner, getHandResult)

import Data.Maybe                       (isNothing)
import Control.Monad                    (when)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.RWS.CPS      (RWST, get, modify, ask, evalRWST)
import Control.Monad.Extra (void)

type GameMonad = RWST Context () GameState IO

playGame :: Context -> GameState -> IO ()
playGame ctx gs = void $ evalRWST gameLoop ctx gs

gameLoop :: GameMonad ()
gameLoop = do
    ctx <- ask
    gs  <- get
    hs  <- liftIO $ initializeHand ctx gs >>= playHand (ctx, gs)
    let res = getHandResult hs
    modify $ updateGameState res
    winner <- getWinner . points <$> get
    liftIO $ printHandResult res winner
    when (isNothing winner) gameLoop
  where
    updateGameState :: PlayerPoints -> GameState -> GameState
    updateGameState (p1', p2') s@GS{ points = (p1, p2), numberOfHands = k } = s
        { points        = (p1 + p1', p2 + p2')
        , numberOfHands = k + 1
        , toStart       = theOther $ toStart s
        }
