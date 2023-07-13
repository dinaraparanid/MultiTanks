# Multi Tanks

2D Tanks multiplayer game based on Apecs (Gloss) graphics library. Client/Server side implementation is based on UDP connection with network library

# Gameplay

Game requires two players to launch. When both players are found, it immediately starts.
Both players try to shoot their opponent (by pressing "Space" button) as fast as possible in order to survive and win.
Game finishes when either first or second player is killed.

# Setup

Before the start, make sure that you have [installed Haskell](https://www.haskell.org/downloads/).

1. Clone repository to your local folder:

```bash
git clone https://github.com/dinaraparanid/MultiTanks
```

2. Firstly, compile and run the server:

```bash
cd MultiTanks/server
stack build
stack exec runghc Server
```

3. Run the client and begin the game:

```bash
cd MultiTanks/game
stack exec runghc Game
```

Game starts when two players are connected

# Preview

![preview](MultiTanks_Preview.gif)