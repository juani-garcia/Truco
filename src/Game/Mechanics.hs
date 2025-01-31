module Game.Mechanics where

import Game.Types
import Game.Utils
import Data.Maybe (maybeToList, isJust)

-- Envido calculations
envido :: CardHand -> Int
envido (c1, c2, c3) = maximum
    [ pairwiseEnvido c1 c2
    , pairwiseEnvido c1 c3
    , pairwiseEnvido c2 c3
    ]
    where
        suited :: Card -> Card -> Bool
        suited ca cb = getSuit ca == getSuit cb

        cardEnvidoValue :: Card -> Int
        cardEnvidoValue c = if n >= 10 then 0 else n
            where n = getNumber c

        suitedEnvidoConstant :: Int
        suitedEnvidoConstant = 20

        pairwiseEnvido :: Card -> Card -> Int
        pairwiseEnvido ca cb = if suited ca cb
            then suitedEnvidoConstant + cardEnvidoValue ca + cardEnvidoValue cb
            else cardEnvidoValue ca `max` cardEnvidoValue cb

calculateEnvidoWinner :: HandState -> Player
calculateEnvidoWinner hs =
    let p1 = startedBy hs
        p2 = theOther p1
        h1 = getPlayerInfo p1 $ hands hs
        h2 = getPlayerInfo p2 $ hands hs
     in case compare (envido h1) (envido h2) of
        LT -> p2
        _  -> p1

-- Cuántos puntos da la falta. El que no juega con estas reglas no sabe jugar al truco.
faltaEnvidoPoints :: PlayerPoints -> Int
faltaEnvidoPoints (p1, p2)
    | m < lasBuenas = requiredPoints     -- Si estamos en las malas
    | otherwise     = requiredPoints - m -- Lo que le falta al que va ganando
  where
    m = p1 `max` p2
    lasBuenas = requiredPoints `div` 2

wasFaltaCalled :: HandState -> Bool
wasFaltaCalled HS{ actions = pas } = any ((==CallFaltaEnvido) . snd) pas

-- *** ROUND MECHANICS ***
requiredPoints :: Int
requiredPoints = 30

analyzeRounds :: Player -> [RoundResult] -> TrucoResult
analyzeRounds starter rs
    | length rs <= 1                                       = TrucoNotFinished
    | wonByPlayer P1 rs >= 2                               = TrucoWonBy P1
    | wonByPlayer P2 rs >= 2                               = TrucoWonBy P2
    | length rs == 2 && head rs /= Tie && rs !! 1 /= Tie   = TrucoNotFinished
    | head rs /= Tie && (rs !! 1 == Tie || rs !! 2 == Tie) = TrucoWonBy (winnerOfRound (head rs)) -- "Primera vale doble"
    | head rs == Tie && rs !! 1 /= Tie                     = TrucoWonBy (winnerOfRound (rs !! 1)) -- "Como parda la mejor"
    | head rs == Tie && rs !! 1 == Tie && rs !! 2 /= Tie   = TrucoWonBy (winnerOfRound (rs !! 2))
    | head rs == Tie && rs !! 1 == Tie && rs !! 2 == Tie   = TrucoWonBy starter                   -- "Gana la mano"
    | otherwise                                            = TrucoNotFinished
   where
    winnerOfRound (RoundWonBy p) = p
    winnerOfRound Tie = error "Unexpected Tie"
    wonByPlayer p rs' = length $ filter (== RoundWonBy p) rs'

analyzeHand :: HandState -> TrucoResult
analyzeHand s@HS { bettingState = HandEnded } = TrucoWonBy $ currentPlayer s -- Analizado después de cambiar de estado
analyzeHand s                                 = analyzeRounds (startedBy s) (roundResults s)

-- TODO: que no se pueda no querer si eso significa perder la partida
possibleActions :: HandState -> [ActionOpt]
possibleActions hs@HS{ bettingState = NoBetting       } = noBettingActions hs
possibleActions hs@HS{ bettingState = EnvidoOffered n } = envidoOfferedActions n $ wasFaltaCalled hs
possibleActions    HS{ bettingState = TrucoOffered  n } = trucoOfferedActions n -- TODO: falta el temita de que el envido va primero...
possibleActions    HS{ bettingState = TrucoAccepted n } = trucoAcceptedActions n
possibleActions    _                                    = error "Estado inválido: no hay posibles acciones."

noBettingActions :: HandState -> [ActionOpt]
noBettingActions hs =
       [P PlayCard]
    ++ [S x |
          null (roundResults hs) && (envidoPoints hs == 0),
          x <- [CallEnvido, CallRealEnvido, CallFaltaEnvido]]
    ++ map S [CallTruco, Fold]

envidoOfferedActions :: Int -> Bool -> [ActionOpt]
envidoOfferedActions n faltaEnvidoCalled = map S $ [Accept, Decline] ++ if faltaEnvidoCalled
    then
        []
    else
        (case n of
            2 -> [CallEnvido, CallRealEnvido]
            4 -> [CallRealEnvido]
            _ -> []) ++ [CallFaltaEnvido] -- Solo para que aparezca última

trucoOfferedActions :: Int -> [ActionOpt]
trucoOfferedActions n = map S [Accept, Decline] ++ raiseTrucoActions n

trucoAcceptedActions :: Int -> [ActionOpt]
trucoAcceptedActions n = [P PlayCard, S Fold] ++ raiseTrucoActions n

raiseTrucoActions:: Int -> [ActionOpt]
raiseTrucoActions n = case n of
    2 -> [S CallReTruco]
    3 -> [S CallValeCuatro]
    4 -> []
    m -> error $ "Estado inválido: se intento un truco de " ++ show m ++ " puntos."

-- Dado el estado actual de la mano, y una acción, devuelve el nuevo estado de apuestas.
-- TODO: probablemente esto se pueda expresar de una forma más concisa, y sin repetir cosas
newBettingState :: HandState -> PlayerPoints -> Action -> Maybe BettingState
newBettingState s                                    _ (PlayCard _)    = Just $ bettingState s
newBettingState HS{ bettingState = NoBetting       } _ CallEnvido      = Just $ EnvidoOffered 2
newBettingState HS{ bettingState = NoBetting       } _ CallRealEnvido  = Just $ EnvidoOffered 3
newBettingState HS{ bettingState = NoBetting       } p CallFaltaEnvido = Just $ EnvidoOffered $ faltaEnvidoPoints p
newBettingState HS{ bettingState = EnvidoOffered n } _ CallEnvido      = Just $ EnvidoOffered (n + 2)
newBettingState HS{ bettingState = EnvidoOffered n } _ CallRealEnvido  = Just $ EnvidoOffered (n + 3)
newBettingState HS{ bettingState = EnvidoOffered _ } p CallFaltaEnvido = Just $ EnvidoOffered $ faltaEnvidoPoints p
newBettingState HS{ bettingState = EnvidoOffered n } _ Accept          = Just $ EnvidoAccepted n
newBettingState HS{ bettingState = EnvidoOffered _ } _ Decline         = Just NoBetting
newBettingState HS{ bettingState = NoBetting       } _ CallTruco       = Just $ TrucoOffered 2
newBettingState HS{ bettingState = TrucoOffered  _ } _ CallReTruco     = Just $ TrucoOffered 3
newBettingState HS{ bettingState = TrucoOffered  _ } _ CallValeCuatro  = Just $ TrucoOffered 4
newBettingState HS{ bettingState = TrucoAccepted 2 } _ CallReTruco     = Just $ TrucoOffered 3
newBettingState HS{ bettingState = TrucoAccepted 3 } _ CallValeCuatro  = Just $ TrucoOffered 4
newBettingState HS{ bettingState = TrucoOffered  n } _ Accept          = Just $ TrucoAccepted n
newBettingState HS{ bettingState = TrucoOffered  _ } _ Decline         = Just HandEnded
newBettingState _                                    _ Fold            = Just HandEnded
newBettingState _                                    _ _               = Nothing

-- El GameState lo necesito para saber los puntos de la mano, para poder calcular cuanto vale la falta.
applyAction :: GameState -> Action -> HandState -> Maybe HandState
applyAction gs a hs = do
    bs <- newBettingState hs (points gs) a 
    let (cp, cr) = case a of
            PlayCard c -> (cardsPlayed hs ++ [(currentPlayer hs, c)], currentRound hs ++ [(currentPlayer hs, c)])
            _          -> (cardsPlayed hs, currentRound hs)
        mr   = getResult cr
        hs' = hs { actions       = actions hs ++ [(currentPlayer hs, a)]
                 , cardsPlayed   = cp
                 , bettingState  = case bs of
                                     EnvidoAccepted _ -> NoBetting -- EnvidoAccepted es un estado efímero.
                                     _                -> bs
                 , currentPlayer = nextPlayer mr hs a
                 , currentRound  = if isJust mr then [] else cr
                 , roundResults  = roundResults hs ++ maybeToList mr
                 , trucoPoints   = updateTrucoPoints bs hs
                 , envidoPoints  = updateEnvidoPoints bs hs
                 , envidoWonBy   = updateEnvidoWinner bs hs
                 , showEnvido    = updateShowEnvido bs hs
                 }

    if isEnvidoAcceptedState bs && hasEnvidoWinnerWon hs'
        then return hs'{ bettingState = HandEnded, trucoPoints = 0 } -- "Early return" si el que ganó el envido gana la partida
        else return hs'
  where
    updateTrucoPoints :: BettingState -> HandState -> Int -- BettingState representa el nuevo estado de apuestas
    updateTrucoPoints (TrucoOffered  n) _ = n - 1
    updateTrucoPoints (TrucoAccepted n) _ = n
    updateTrucoPoints _                 s = trucoPoints s

    updateEnvidoPoints :: BettingState -> HandState -> Int
    updateEnvidoPoints (EnvidoOffered  _) HS{ bettingState = EnvidoOffered n } = n
    updateEnvidoPoints (EnvidoOffered  _) _                                    = 1
    updateEnvidoPoints (EnvidoAccepted m) _                                    = m
    updateEnvidoPoints _                  s                                    = envidoPoints s

    updateEnvidoWinner :: BettingState -> HandState -> Maybe Player
    updateEnvidoWinner (EnvidoAccepted _) s = Just $ calculateEnvidoWinner s
    updateEnvidoWinner (EnvidoOffered  _) s = Just $ currentPlayer s
    updateEnvidoWinner _                  s = envidoWonBy s

    updateShowEnvido :: BettingState -> HandState -> Bool
    updateShowEnvido (EnvidoAccepted _) _ = True
    updateShowEnvido _                  s = showEnvido s

    nextPlayer :: Maybe RoundResult -> HandState -> Action -> Player
    nextPlayer (Just (RoundWonBy p)) _  _      = p
    nextPlayer (Just Tie)            s  _      = startedBy s
    nextPlayer _                     s  Accept = nextInRound s
    nextPlayer _                     s  _      = theOther $ currentPlayer s

    -- El próximo jugador siguiendo el orden de la ronda (ie, quién tiene que tirar)
    nextInRound :: HandState -> Player
    nextInRound s = case cardsPlayed s of
        [] -> startedBy s
        xs -> theOther $ fst (last xs)

    -- Si el que ganó el envido tiene más de 30 se termina la mano
    hasEnvidoWinnerWon :: HandState -> Bool
    hasEnvidoWinnerWon HS{ envidoWonBy = Just p, envidoPoints = n } =
        let m = getPlayerInfo p $ points gs
        in (m + n) >= requiredPoints
    hasEnvidoWinnerWon _ = False

    isEnvidoAcceptedState (EnvidoAccepted _) = True
    isEnvidoAcceptedState _                  = False

getHandPoints :: Player -> HandState -> Int
getHandPoints p hs = getTrucoPoints p hs + getEnvidoPoints p hs
    where
        getTrucoPoints q s = case analyzeHand s of
            TrucoWonBy q'      -> if q == q' then trucoPoints s else 0
            TrucoNotFinished   -> error "No se pueden pedir puntos de una mano que no finalizó."
        getEnvidoPoints q s
            | envidoWonBy s == Just q = envidoPoints s
            | otherwise               = 0

getHandResult :: HandState -> PlayerPoints
getHandResult hs = (getHandPoints P1 hs, getHandPoints P2 hs)

getWinner :: PlayerPoints -> Maybe Player
getWinner (p1, p2)
    | p1 >= requiredPoints = Just P1
    | p2 >= requiredPoints = Just P2
    | otherwise = Nothing
