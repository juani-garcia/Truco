module Game.Encoders where

import Data.Word    (Word8)
import Data.List    (elemIndex)
import Game.Deck    (createDeck)
import Game.Types

cardIndex :: Card -> Int
cardIndex c = case elemIndex c createDeck of 
    Just i  -> i
    Nothing -> error "Carta no encontrada en el mazo."

indexCard :: Int -> Card
indexCard i = 
    if i >= 0 && i < 40 
        then createDeck !! i 
        else error "Índice fuera de rango."

encode :: Action -> Word8
encode action = case action of
    PlayCard c      -> fromIntegral $ cardIndex c
    CallEnvido      -> 100
    CallRealEnvido  -> 101
    CallFaltaEnvido -> 102
    CallTruco       -> 103
    CallReTruco     -> 104
    CallValeCuatro  -> 105
    Accept          -> 200
    Decline         -> 201
    Fold            -> 202

decode :: Word8 -> Action
decode w = case w of
    100 -> CallEnvido
    101 -> CallRealEnvido
    102 -> CallFaltaEnvido
    103 -> CallTruco
    104 -> CallReTruco
    105 -> CallValeCuatro
    200 -> Accept
    201 -> Decline
    202 -> Fold
    n   -> if n >= 0 && n < 40
        then PlayCard $ indexCard $ fromIntegral n
        else error "Código de acción inválido."
