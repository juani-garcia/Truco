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

-- *** ROUND MECHANICS ***
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

possibleActions :: HandState -> [ActionOpt]
possibleActions hs@HS{ bettingState = NoBetting       } = noBettingActions hs
possibleActions    HS{ bettingState = EnvidoOffered n } = envidoOfferedActions n
possibleActions    HS{ bettingState = TrucoOffered  n } = trucoOfferedActions n
possibleActions    HS{ bettingState = TrucoAccepted n } = trucoAcceptedActions n
possibleActions    hs                                   = error $ "Estado inválido: no hay posibles acciones. El estado era: " ++ show hs

noBettingActions :: HandState -> [ActionOpt]
noBettingActions hs =
       [P PlayCard]
    ++ [S CallEnvido | null (roundResults hs) && (envidoPoints hs == 0)]
    ++ map S [CallTruco, Fold]

envidoOfferedActions :: Int -> [ActionOpt]
envidoOfferedActions n = map S $ [Accept, Decline] ++ case n of
    2 -> [CallEnvido, CallRealEnvido]
    4 -> [CallRealEnvido]
    5 -> []
    7 -> []
    m -> error $ "Estado inválido: se intentó ofrecer un envido por " ++  show m ++ " puntos."

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

-- Dado el estado actual de la mano, y una acción, devuelve el nuevo estado de apuestas
newBettingState :: HandState -> Action -> Maybe BettingState
newBettingState s                                    (PlayCard _)   = Just $ bettingState s
newBettingState HS{ bettingState = NoBetting       } CallEnvido     = Just $ EnvidoOffered 2
newBettingState HS{ bettingState = NoBetting       } CallRealEnvido = Just $ EnvidoOffered 3
newBettingState HS{ bettingState = EnvidoOffered n } CallEnvido     = Just $ EnvidoOffered (n + 2)
newBettingState HS{ bettingState = EnvidoOffered n } CallRealEnvido = Just $ EnvidoOffered (n + 3)
newBettingState HS{ bettingState = EnvidoOffered n } Accept         = Just $ EnvidoAccepted n
newBettingState HS{ bettingState = EnvidoOffered _ } Decline        = Just NoBetting
newBettingState HS{ bettingState = NoBetting       } CallTruco      = Just $ TrucoOffered 2
newBettingState HS{ bettingState = TrucoOffered _  } CallReTruco    = Just $ TrucoOffered 3
newBettingState HS{ bettingState = TrucoOffered _  } CallValeCuatro = Just $ TrucoOffered 4
newBettingState HS{ bettingState = TrucoOffered n  } Accept         = Just $ TrucoAccepted n
newBettingState HS{ bettingState = TrucoOffered _  } Decline        = Just HandEnded
newBettingState _                                    Fold           = Just HandEnded
newBettingState _                                    _              = Nothing

applyAction :: HandState -> Action -> Maybe HandState
applyAction hs a = do
    bs <- newBettingState hs a
    let (cp, cr) = case a of
            PlayCard c -> (cardsPlayed hs ++ [(currentPlayer hs, c)], currentRound hs ++ [(currentPlayer hs, c)])
            _          -> (cardsPlayed hs, currentRound hs)
        mr   = getResult cr
    return hs { actions       = actions hs ++ [(currentPlayer hs, a)]
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

getHandPoints :: Player -> HandState -> Int
getHandPoints p hs = getTrucoPoints p hs + getEnvidoPoints p hs
    where
        getTrucoPoints q s = case analyzeHand s of
            TrucoWonBy q' -> if q == q' then trucoPoints s else 0
            TrucoNotFinished   -> error "No se pueden pedir puntos de una mano que no finalizó."
        getEnvidoPoints q s
            | envidoWonBy s == Just q = envidoPoints s
            | otherwise               = 0
        

