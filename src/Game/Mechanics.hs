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

analyzeHands :: HandState -> HandResult
analyzeHands s@HS { bettingState = HandEnded } = HandWonBy $ nextToPlay s
analyzeHands s                                 = analyzeRounds (startedBy s) (roundResults s)

possibleActions :: HandState -> [ActionOpt]
possibleActions hs
    | bs == NoBetting     = [P PlayCard, S CallTruco, S Fold]
    | bs == TrucoOffered  = [S Accept, S Decline]
    | bs == TrucoAccepted = [P PlayCard, S Fold]
    | otherwise           = []
    where
        bs = bettingState hs

-- Dado el estado actual de la mano, y una acción, devuelve el nuevo estado de apuestas
newBettingState :: HandState -> Action -> Maybe BettingState
newBettingState s                                 (PlayCard _) = Just $ bettingState s
newBettingState HS{ bettingState = NoBetting }    CallTruco    = Just TrucoOffered
newBettingState HS{ bettingState = TrucoOffered } Accept       = Just TrucoAccepted
newBettingState HS{ bettingState = TrucoOffered } Decline      = Just HandEnded
newBettingState _                                 Fold         = Just HandEnded
newBettingState _                                 _            = Nothing

applyAction :: HandState -> Action -> Maybe HandState
applyAction s a = do
    bs <- newBettingState s a
    let (cp, cr) = case a of
            PlayCard c -> (cardsPlayed s ++ [(nextToPlay s, c)], currentRound s ++ [(nextToPlay s, c)])
            _          -> (cardsPlayed s, currentRound s)
        mr  = getResult cr
        rs = roundResults s ++ maybeToList mr
        next = case mr of
            Just (RoundWonBy p) -> p
            Just Tie            -> startedBy s
            Nothing             -> theOther $ nextToPlay s 
    return s { actions      = actions s ++ [(nextToPlay s, a)]
             , cardsPlayed  = cp
             , bettingState = bs
             , nextToPlay   = next
             , currentRound = if length cr == 2 then [] else cr
             , roundResults = rs
             }
