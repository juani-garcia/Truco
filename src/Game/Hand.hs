module Game.Hand (playHand) where

import Game.Types
import Game.Mechanics
import Game.CLI
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class

type HandMonad = StateT HandState IO

playHand :: HandState -> IO HandState
playHand = evalStateT loop

loop :: HandMonad HandState
loop = do
    hs <- get
    liftIO $ printHandState hs
    action <- liftIO $ getAction (gameState hs) hs
    modify (newState action)
    hs' <- get
    case analyzeHand hs' of
        TrucoNotFinished -> loop
        _                -> return hs'
  where
    newState action hs  = case applyAction hs action of
        Nothing  -> error $ "Acción inválida. La acción problemática es: " ++ show action
        Just hs' -> hs'

