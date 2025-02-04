module Game.Hand (playHand) where

import Game.Types
import Game.Mechanics
import Game.CLI
import Control.Monad.IO.Class           (MonadIO(liftIO))
import Control.Monad.Trans.RWS.Strict   (RWST, get, gets, modify, ask, evalRWST)

type HandMonad = RWST (GameAgent, GameState) () HandState IO

playHand :: (GameAgent, GameState) -> HandState -> IO PlayerPoints
playHand handCtx hs = fst <$> evalRWST handLoop handCtx hs

handLoop :: HandMonad PlayerPoints
handLoop = do
    (agent, gs) <- ask
    hs <- get
    liftIO $ printHandState gs hs
    action <- liftIO $ getAction agent hs
    modify (newState gs action)
    res <- gets analyzeHand
    case res of
        TrucoNotFinished -> handLoop
        _                -> gets getHandResult
  where
    newState gs action hs  = case applyAction gs action hs of
        Nothing  -> error $ "Acción inválida. La acción problemática es: " ++ show action
        Just hs' -> hs'
