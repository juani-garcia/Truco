module Main where

import Game.Hand (playHand)

main :: IO ()
main = do
    putStrLn "Bienvenido al juego de truco. Pulse ENTER para comenzar."
    _ <- getLine
    playHand
