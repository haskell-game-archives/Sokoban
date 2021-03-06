Sokoban
=======

[![cabal](https://github.com/haskell-game-archives/Sokoban/workflows/cabal/badge.svg)](https://github.com/haskell-game-archives/Sokoban/actions?query=workflow%3Acabal)
[![stack](https://github.com/haskell-game-archives/Sokoban/workflows/stack/badge.svg)](https://github.com/haskell-game-archives/Sokoban/actions?query=workflow%3Astack)
[![lint](https://github.com/haskell-game-archives/Sokoban/workflows/lint/badge.svg)](https://github.com/haskell-game-archives/Sokoban/actions?query=workflow%3Alint)

A Sokoban implementation using Haskell and Gloss.

Currently uses David W. Skinner's Sasquatch packs for levels: http://www.abelmartin.com/rj/sokobanJS/Skinner/David%20W.%20Skinner%20-%20Sokoban.htm

Controls:

* Move: Arrow keys

* Retry: r

If you wish to load a new pack of levels from the sasquatch pack, simply run:

    $ ./sokoban sasquatch "filename.txt"


which should parse and load the new pack, but note: this will wipe any saved progress.

To reset your progress:

    $ ./sokoban reset

To play from a certain level:

    $ ./sokoban level [level number]

Note: When playing like this, your progress will not save.
