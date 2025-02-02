module Game.Connection where

import Game.Types
import Game.Utils
import Game.CLI
import Game.Deck                    (deal)
import Game.Encoders                (encode, decode)

import qualified Control.Exception as E
import qualified Data.ByteString as BS

import Network.Socket.ByteString    (recv, sendAll)
import System.IO.Error              (isDoesNotExistError, isPermissionError, isAlreadyInUseError, isUserError, ioeGetErrorString)
import Network.Socket hiding        (defaultPort)
import System.Exit                  (exitFailure)
import Control.Exception            (throwIO)
import Control.Monad.Extra          (when)
import Data.Word                    (Word8)

defaultPort :: ServiceName
defaultPort = "3333"

safeRecv :: Socket -> Int -> IO [Word8]
safeRecv sock n = do
    msg <- BS.unpack <$> recv sock n
    when (null msg) $ throwIO (userError "Tu contrincante cerró la conexión.")
    return msg

safeSend :: Socket -> [Word8] -> IO ()
safeSend sock msg = do
    E.catch (sendAll sock $ BS.pack msg) 
            (\(E.SomeException _) -> throwIO (userError "No se pudo enviar el mensaje a tu contrincante."))

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
        head <$> getAddrInfo (Just hints) Nothing (Just defaultPort)

    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1
        return sock

    awaitConn sock = do
        (conn, _peer) <- accept sock
        return conn

connectToPlayer :: IO Socket
connectToPlayer = do
    putStrLn "Ingrese la IP y puerto de su contrincante (ip:puerto)."
    input <- getLine
    let (host, port) = parseInput input
    addr <- resolve host port
    open addr
  where
    parseInput input =
        let (host, portPart) = break (== ':') input
        in if null portPart
            then (host, defaultPort)
            else (host, drop 1 portPart)

    resolve host port = do
        let hints = defaultHints {
              addrSocketType = Stream
            }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)

    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

initializeViaSocket :: Socket -> GameState -> IO HandState
initializeViaSocket sock gs = do
    (h1, h2) <- getHand
    return $ initialState h1 h2 $ toStart gs
  where
    getHand = if toStart gs == P1 then sendHand else recvHand

    sendHand :: IO (CardHand, CardHand)
    sendHand = do
        putStrLn "Sos mano, mandándole las cartas a tu contrincante..."
        idxs <- chooseRandomIndices
        safeSend sock $ map fromIntegral idxs
        return $ deal idxs

    recvHand :: IO (CardHand, CardHand)
    recvHand = do
        putStrLn "Tu contrincante es mano. Esperando que te mande las cartas..."
        idxs <- safeRecv sock 6
        return $ deal $ reverse $ map fromIntegral idxs

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
        msg <- BS.unpack <$> recv sock 1
        when (null msg) $ throwIO (userError "Tu contrincante cerró la conexión.")
        return $ decode $ head msg

handleException :: E.IOException -> IO ()
handleException ex = do 
    putStrLn $ humanReadable ex
    exitFailure

humanReadable :: E.IOException -> String
humanReadable ioe
  | isDoesNotExistError ioe = "Error: el recurso solicitado no existe. Verifica la dirección o puerto."
  | isPermissionError   ioe = "Error: permiso denegado. Intenta ejecutar el programa con privilegios de administrador."
  | isAlreadyInUseError ioe = "Error: el puerto ya está en uso. Prueba con otro puerto."
  | isUserError         ioe = "Error: " ++ ioeGetErrorString ioe
  | otherwise               = "Error inesperado estableciendo la conexión: " ++ show ioe
