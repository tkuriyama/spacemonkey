# Spacemonkey Server

The Spacemonkey server is currently powered by [`Servant`](https://github.com/haskell-servant/servant), with the [`servant-elm`](https://hackage.haskell.org/package/servant-elm) library wrapping `elm-bridge` for Elm code gen. Persistence remains TBD... 


## Build

The build commands are wrapped with [redo](https://redo.readthedocs.io/en/latest/). Running `redo all` in the directory root triggers rebuilds, but `redo` is not required. The contents of all the `all.do` files can be run as individual shell commands.

<hr>

## Server Instances

### HelloServer

`HelloServer` is Spacemonkey's Hello World. It doesn't have persistence, instead using an `IORef` to maintain the counter state as the client updates it.

### HelloServerAcid

An iteration of `HelloServer` that uses [`acid-state`](https://github.com/acid-state) for persistence of native Haskell data structures.

### HelloServerPersistent

An iteration of `HelloServer` that uses [`persistent`](https://github.com/yesodweb/persistent) for persistence in the form of a SQL database (in this case, Sqlite).

