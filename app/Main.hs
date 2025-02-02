module Main where

import Network.Socket       (close)
import Control.Exception    (handle)
import Game.Core            (playGame)
import Options              (getOptions)
import Game.Connection      (getConnection, agent, handleException)


main :: IO ()
main = do
    opts <- getOptions
    handle handleException $ do
        (sock, starter) <- getConnection opts
        playGame (agent sock) starter
        close sock
    