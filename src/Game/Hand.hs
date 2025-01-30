module Game.Hand (playHand) where

import Game.Types
import Game.Mechanics
import Game.CLI

handLoop :: HandState -> IO HandState
handLoop hs = do
    printHandState hs
    action <- getAction (gameState hs) hs
    let ms = applyAction hs action
    case ms of
        Nothing -> error $ "Acción inválida. La acción problemática es: " ++ show action
        Just hs' -> do
            let result = analyzeHand hs'
            case result of
                TrucoNotFinished -> handLoop hs'
                _                -> return hs'

playHand :: HandState -> IO HandState
playHand = handLoop
