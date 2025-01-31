module Game.Hand where

import Game.Types
import Game.Mechanics
import Game.CLI
import Control.Monad.Trans.RWS.CPS      (RWST, get, modify, ask, evalRWST)
import Control.Monad.IO.Class           (MonadIO(liftIO))

type HandMonad = RWST (Context, GameState) () HandState IO

playHand :: (Context, GameState) -> HandState -> IO HandState
playHand cts hs = fst <$> evalRWST handLoop cts hs

handLoop :: HandMonad HandState
handLoop = do
    (ctx, gs) <- ask
    hs <- get
    liftIO $ printHandState gs hs
    action <- liftIO $ getAction ctx hs
    modify (newState gs action)
    res <- analyzeHand <$> get
    case res of
        TrucoNotFinished -> handLoop
        _                -> get
  where
    newState gs action hs  = case applyAction gs action hs of
        Nothing  -> error $ "Acción inválida. La acción problemática es: " ++ show action
        Just hs' -> hs'
