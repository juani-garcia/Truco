module Game.Connection where

import Game.Types
import Game.Utils
import Game.CLI
import Game.Deck                    (deal)
import Game.Encoders                (encode, decode)

import Network.Socket
import qualified Control.Exception as E
import qualified Data.ByteString as BS

import Control.Monad                (unless)
import Network.Socket.ByteString    (recv, sendAll)

host :: HostName
host = "localhost" -- TODO: que esto sea configurable

port :: ServiceName
port = "3333"

awaitForPlayer :: IO Socket
awaitForPlayer = do
    addr <- resolve
    E.bracket (open addr) close awaitConn
  where
    resolve = do
        let hints = defaultHints  {
              addrFlags      = [AI_PASSIVE]
            , addrSocketType = Stream
            }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)

    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1
        return sock

    awaitConn sock = do
        (conn, _peer) <- accept sock
        msg <- recv conn 1
        let expected = BS.pack [0]
        unless (msg == expected) $ error "El otro jugador no envió lo esperado"
        sendAll conn msg
        return conn

connectToPlayer :: IO Socket
connectToPlayer = do
    addr <- resolve
    open addr
  where
    resolve = do
        let hints = defaultHints {
              addrSocketType = Stream
            }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        let msg = BS.pack [0]
        sendAll sock msg
        reply <- recv sock 1
        unless (msg == reply) $ error "El otro jugador no envió lo esperado."
        return sock

initializeViaSocket :: Socket -> GameState -> IO HandState
initializeViaSocket sock gs = do
    (h1, h2) <- getHand
    return $ initialState h1 h2 gs
  where
    getHand = if toStart gs == P1 then sendHand else recvHand

    sendHand :: IO (CardHand, CardHand)
    sendHand = do
        putStrLn "Sos mano, mandándole las cartas a tu contrincante..."
        idxs <- chooseRandomIndices
        sendAll sock $ BS.pack $ map fromIntegral idxs
        return $ deal idxs

    recvHand :: IO (CardHand, CardHand)
    recvHand = do
        putStrLn "Tu contrincante es mano. Esperando que te mande las cartas..."
        bsIdxs <- recv sock 6
        let idxs = map fromIntegral $ BS.unpack bsIdxs
        return $ deal $ reverse idxs

getActionViaSocket :: Socket -> HandState -> IO Action
getActionViaSocket sock hs = do
    if currentPlayer hs == P1
        then getLocallyAndSend
        else receiveAction
  where
    getLocallyAndSend = do
        action <- getActionLocally hs
        sendAll sock $ BS.pack [encode action]
        return action

    receiveAction = do
        putStrLn "Esperando que juegue tu contrincante..."
        decode . head . BS.unpack <$> recv sock 1
