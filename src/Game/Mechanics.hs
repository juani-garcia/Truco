module Game.Mechanics where

import Game.Types
import Game.Utils
import Data.Maybe (maybeToList)

-- Envido calculations
envido :: CardHand -> Int
envido (c1, c2, c3) = maximum
    [ pairwiseEnvido c1 c2
    , pairwiseEnvido c1 c3
    , pairwiseEnvido c2 c3
    ]
    where
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
analyzeRounds :: Player -> [RoundResult] -> HandResult
analyzeRounds starter rs
    | length rs <= 1                                       = NotFinished
    | wonByPlayer P1 rs >= 2                               = HandWonBy P1
    | wonByPlayer P2 rs >= 2                               = HandWonBy P2
    | length rs == 2 && head rs /= Tie && rs !! 1 /= Tie   = NotFinished
    | head rs /= Tie && (rs !! 1 == Tie || rs !! 2 == Tie) = HandWonBy (winnerOfRound (head rs)) -- "Primera vale doble"
    | head rs == Tie && rs !! 1 /= Tie                     = HandWonBy (winnerOfRound (rs !! 1)) -- "Como parda la mejor"
    | head rs == Tie && rs !! 1 == Tie && rs !! 2 /= Tie   = HandWonBy (winnerOfRound (rs !! 2))
    | head rs == Tie && rs !! 1 == Tie && rs !! 2 == Tie   = HandWonBy starter                   -- "Gana la mano"
    | otherwise                                            = NotFinished
    where
        winnerOfRound (RoundWonBy p) = p
        winnerOfRound Tie = error "Unexpected Tie"
        wonByPlayer p rs' = length $ filter (== RoundWonBy p) rs'

analyzeHand :: HandState -> HandResult
analyzeHand s@HS { bettingState = HandEnded } = HandWonBy $ currentPlayer s
analyzeHand s                                 = analyzeRounds (startedBy s) (roundResults s)

possibleActions :: HandState -> [ActionOpt]
possibleActions hs
    | bs == NoBetting && null rs && ep == 0 = [P PlayCard, S CallEnvido, S CallTruco, S Fold] -- Envido solo se puede cantar en primera
    | bs == NoBetting                       = [P PlayCard, S CallTruco, S Fold]
    | bs == EnvidoOffered                   = [S Accept, S Decline]
    | bs == TrucoOffered                    = [S Accept, S Decline]
    | bs == TrucoAccepted                   = [P PlayCard, S Fold]
    | otherwise                             = []
    where
        bs = bettingState hs
        rs = roundResults hs
        ep = envidoPoints hs

-- Dado el estado actual de la mano, y una acción, devuelve el nuevo estado de apuestas
newBettingState :: HandState -> Action -> Maybe BettingState
newBettingState s                                  (PlayCard _) = Just $ bettingState s
newBettingState HS{ bettingState = NoBetting }     CallEnvido   = Just EnvidoOffered
newBettingState HS{ bettingState = EnvidoOffered } Accept       = Just EnvidoAccepted
newBettingState HS{ bettingState = EnvidoOffered } Decline      = Just NoBetting
newBettingState HS{ bettingState = NoBetting }     CallTruco    = Just TrucoOffered
newBettingState HS{ bettingState = TrucoOffered }  Accept       = Just TrucoAccepted
newBettingState HS{ bettingState = TrucoOffered }  Decline      = Just HandEnded
newBettingState _                                  Fold         = Just HandEnded
newBettingState _                                  _            = Nothing

applyAction :: HandState -> Action -> Maybe HandState
applyAction hs a = do
    bs <- newBettingState hs a
    let (cp, cr) = case a of
            PlayCard c -> (cardsPlayed hs ++ [(currentPlayer hs, c)], currentRound hs ++ [(currentPlayer hs, c)])
            _          -> (cardsPlayed hs, currentRound hs)
        mr   = getResult cr
    return hs { actions       = actions hs ++ [(currentPlayer hs, a)]
              , cardsPlayed   = cp
              , bettingState  = if bs == EnvidoAccepted then NoBetting else bs
              , currentPlayer = nextPlayer mr hs
              , currentRound  = if length cr == 2 then [] else cr
              , roundResults  = roundResults hs ++ maybeToList mr
              , trucoPoints   = updateTrucoPoints bs hs
              , envidoPoints  = updateEnvidoPoints bs hs
              , envidoWonBy   = updateEnvidoWinner bs hs
              }

updateTrucoPoints :: BettingState -> HandState -> Int -- BettingState representa el nuevo estado de apuestas
updateTrucoPoints bs hs = if bs == TrucoAccepted then 2 else trucoPoints hs

updateEnvidoPoints :: BettingState -> HandState -> Int
updateEnvidoPoints EnvidoOffered  _ = 1
updateEnvidoPoints EnvidoAccepted _ = 2
updateEnvidoPoints _              s = envidoPoints s

updateEnvidoWinner :: BettingState -> HandState -> Maybe Player
updateEnvidoWinner EnvidoAccepted hs = Just $ calculateEnvidoWinner hs        
updateEnvidoWinner EnvidoOffered  hs = Just $ currentPlayer hs
updateEnvidoWinner _              hs = envidoWonBy hs

nextPlayer :: Maybe RoundResult -> HandState -> Player
nextPlayer (Just (RoundWonBy p)) _  = p
nextPlayer (Just Tie)            hs = startedBy hs
nextPlayer _                     hs = theOther $ currentPlayer hs    

getTrucoPoints :: Player -> HandState -> Int
getTrucoPoints p s = case analyzeHand s of
    HandWonBy p' -> if p == p' then trucoPoints s else 0
    NotFinished  -> error "No se pueden pedir puntos de una mano que no finalizó."

getEnvidoPoints :: Player -> HandState -> Int
getEnvidoPoints p hs
    | envidoWonBy hs == Just p = envidoPoints hs
    | otherwise             = 0

getHandPoints :: Player -> HandState -> Int
getHandPoints p s = getTrucoPoints p s + getEnvidoPoints p s
