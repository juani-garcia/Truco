module Utils where

import Data.Array.IO
import System.Random (randomRIO)
import Control.Monad (forM)

shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- ioArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
    where
        n = length xs
        ioArray :: Int -> [a] -> IO (IOArray Int a)
        ioArray l = newListArray (1,l)