module Options (getOptions) where

import Options.Applicative
import Game.Types

optionsParser :: Parser Options
optionsParser = Options
    <$> optional (strOption
        (  long "host"
        <> short 'h'
        <> metavar "HOST"
        <> help "Host sobre el cual realizar la conexión. Por default, hace el bind sobre todas las interfaces en el caso listen, \
                \y se conecta a localhost en el caso connect."
        ))
    <*> optional (strOption
        (  long "port"
        <> short 'p'
        <> metavar "PORT"
        <> help "Puerto sobre el cual realizar la conexión. Por default es el 3333"
        ))
    <*> switch
        (  long "listen"
        <> short 'l'
        <> help "Espera por conexiones entrantes."
        )
    <*> switch
        (  long "connect"
        <> short 'c'
        <> help "Espera por conexiones entrantes."
        )

options :: ParserInfo Options
options = info (optionsParser <**> helper)
    (  fullDesc
    <> progDesc "Opciones para el juego."
    <> header "Juego de truco para dos jugadores.")

getOptions :: IO Options
getOptions = execParser options
