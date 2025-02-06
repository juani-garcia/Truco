# Truco-hs

Este es un juego de Truco para dos jugadores, implementado en completamente en Haskell.
## Instalación

Es requisito tener instalado [Cabal](https://www.haskell.org/cabal/). Una vez instalado, se debe clonar el repositorio. Para construir el ejecutable, se debe correr el siguiente comando.

```sh
cabal build
```

Para ejecutar el programa, primero tenés que encontrar la ruta del ejecutable con:

```sh
cabal list-bin exe:truco
```

Luego, lo podés ejecutar con:

```sh
$(cabal list-bin exe:truco) [OPCIONES]
```

También se puede correr directamente utilizando `cabal run`, pasando las opciones con `--`.

## Uso

Al ejecutar el programa sin argumentos, se iniciará en modo interactivo y te pedirá que especifiques si deseas escuchar conexiones o conectarte a otro jugador. Por default, utilizará el puerto 3333 de localhost (en el caso pasivo, se hará el bind sobre todas las interfaces de red).

También se pueden proporcionar opciones en la línea de comandos:

```sh
$(cabal list-bin exe:truco) [OPCIONES]
```

### Opciones disponibles

| Opción               | Descripción |
|----------------------|-------------|
| `-h, --host HOST`   | Host al que conectarse. Si no se especifica, en modo `listen` se enlaza a todas las interfaces y en modo `connect` se conecta a `localhost`. |
| `-p, --port PORT`   | Puerto sobre el cual realizar la conexión. El valor por defecto es `3333`. |
| `-n, --name NAME`   | Tu nombre en la partida. |
| `-l, --listen`      | Espera conexiones entrantes. |
| `-c, --connect`     | Se conecta a otro jugador. |

## Requisitos

- GHC y Cabal
- Conexión de red para el modo multijugador

