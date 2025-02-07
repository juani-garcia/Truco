module Game.Connection
    ( getConnection
    , awaitForPlayer
    , connectToPlayer
    , initializeViaSocket
    , getActionViaSocket
    , agent
    , handleException) where

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
import Control.Applicative          (Alternative((<|>)))
import Control.Monad.Extra          (when)
import Data.Word                    (Word8)
import Data.Maybe                   (fromMaybe)
import Data.Char                    (ord, chr)


getConnection :: Options -> IO (Socket, GameState)
getConnection opts = do
    when (connectFlag opts && listenFlag opts) $ throwIO (userError "Los flags -c y -l son mutuamente excluyentes.")
    name <- getName (mname opts)
    if listenFlag opts
        then awaitForPlayer name (mhost opts) (mport opts)
    else if connectFlag opts
        then connectToPlayer name (mhost opts) (mport opts)
    else menu (awaitForPlayer name Nothing Nothing) (connectToPlayer name Nothing Nothing)
  where
    validateName n
        | length n <= 16 = return n
        | otherwise = do
            putStrLn "¡El nombre es muy largo! Como máximo 16 caracteres."
            promptForName

    promptForName = do
        putStrLn "Ingrese su nombre (máximo 16 caracteres):"
        name <- getLine
        validateName name

    getName = maybe promptForName validateName

defaultPort :: ServiceName
defaultPort = "3333"

defaultHost :: HostName
defaultHost = "localhost"

safeRecv :: Socket -> Int -> IO [Word8]
safeRecv sock n = do
    msg <- BS.unpack <$> recv sock n
    when (null msg) $ throwIO (userError "Tu contrincante cerró la conexión.")
    return msg

safeSend :: Socket -> [Word8] -> IO ()
safeSend sock msg = do
    E.catch (sendAll sock $ BS.pack msg)
            (\(E.SomeException _) -> throwIO (userError "No se pudo enviar el mensaje a tu contrincante."))

initialGameState :: Player -> Name -> Name -> GameState
initialGameState p n1 n2 = GS {
      points        = (0, 0)
    , numberOfHands = 1
    , toStart       = p
    , names         = (n1, n2)
    }

awaitForPlayer :: Name -> Maybe HostName -> Maybe ServiceName -> IO (Socket, GameState)
awaitForPlayer name mh mp = do
    addr <- resolve
    E.bracket (open addr) close awaitConn
  where
    resolve = do
        let hints = defaultHints  {
              addrFlags      = [AI_PASSIVE]
            , addrSocketType = Stream
            }
        head <$> getAddrInfo (Just hints) mh (mp <|> Just defaultPort)

    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1
        return sock

    awaitConn sock = do
        let host = fromMaybe "*" mh
            port = fromMaybe defaultPort mp
        putStrLn $ "Esperando conexiones en " ++ host ++ ":" ++ port
        (conn, _peer) <- accept sock
        rivalName <- map (chr . fromIntegral) <$> safeRecv conn 16
        safeSend conn $ map (fromIntegral . ord) name
        putStrLn $ "¡" ++ rivalName ++ " se conectó exitosamente!"
        return (conn, initialGameState P1 name rivalName)

connectToPlayer :: Name -> Maybe HostName -> Maybe ServiceName -> IO (Socket, GameState)
connectToPlayer name mh mp = do
    addr <- resolve
    open addr
  where
    resolve = do
        let hints = defaultHints {
              addrSocketType = Stream
            }
        head <$> getAddrInfo (Just hints) (mh <|> Just defaultHost) (mp <|> Just defaultPort)

    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        let host = fromMaybe "localhost" mh
            port = fromMaybe defaultPort mp
        putStrLn $ "Intentando conectarse a " ++ host ++ ":" ++ port
        connect sock $ addrAddress addr
        safeSend sock $ map (fromIntegral . ord) name
        rivalName <- map (chr . fromIntegral) <$> safeRecv sock 16
        putStrLn $ "¡" ++ rivalName ++ " se conectó exitosamente!"
        return (sock, initialGameState P2 name rivalName)

agent :: Socket -> GameAgent
agent sock = GameAgent
    { initializeHand = initializeViaSocket sock
    , getAction      = getActionViaSocket sock
    }

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
