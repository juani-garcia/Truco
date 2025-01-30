module Game.Core (playGame) where

import Game.Hand
import Game.Mechanics   (getWinner, getHandResult)
import Game.Types
import Game.Utils       (theOther)
import Game.CLI         (printHandResult)
import Data.Maybe       (isNothing)
import Control.Monad    (when)

gameLoop :: GameState -> IO ()
gameLoop gs = do
    finalState <- initializeHand gs gs >>= playHand
    let res    = getHandResult finalState
        gs'    = updateGameState gs res
        winner = getWinner $ points gs'
    
    printHandResult res winner
    when (isNothing winner) $ gameLoop gs'

    where
        updateGameState :: GameState -> PlayerPoints -> GameState
        updateGameState s@GS{ points = (p1, p2), numberOfHands = k } (p1', p2') = s
            { points        = (p1 + p1', p2 + p2')
            , numberOfHands = k + 1
            , toStart       = theOther $ toStart gs
            }

playGame :: GameState -> IO ()
playGame = gameLoop
