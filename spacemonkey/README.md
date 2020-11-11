# Spacemonkey Server

A small social network server.

The Spacemonkey server is currently powered by [`Servant`](https://github.com/haskell-servant/servant), with the [`servant-elm`](https://hackage.haskell.org/package/servant-elm) library wrapping `elm-bridge` for Elm code gen. Persistence is achieved with [`persistent-sqlite`](https://hackage.haskell.org/package/persistent-sqlite), though it is possible to use a different database supported by `persistent` without too much work.


## Build

The build commands are wrapped with [redo](https://redo.readthedocs.io/en/latest/). Running `redo all` in the directory root triggers rebuilds, but `redo` is not required. The contents of all the `all.do` files can be run as individual shell commands (e.g. `stack install`).

Note that the build script (see above) will build whichever server is specified in `server/Main.hs`, which also specifies the base URL that the front-end Elm code expects.

<hr>

## Hello Server Instances

### HelloServer

`HelloServer` is Spacemonkey's Hello World. It doesn't have persistence, instead using an `IORef` to maintain the counter state as the client updates it.

### HelloServerAcid

An iteration of `HelloServer` that uses [`acid-state`](https://github.com/acid-state) for persistence of native Haskell data structures.

### HelloServerPersistent

An iteration of `HelloServer` that uses [`persistent`](https://github.com/yesodweb/persistent) for persistence in the form of a SQL database (in this case, Sqlite).


## Spacmonkey Server

The goal of Spacemonkey's design is to minimize friction across data boundaries. Accordingly, the applications core data structures are defined only once, in `src/Modules/Spacemonkey.hs`. The same file also defines that REST APIs for Servant, though the implementation of the server-side logic for handling the APIs resides in `server/Server/Spacemonkey.hs`.

* Front-end: the `servant-elm` library is used to automatically generate all Elm data types and de/serializers that correspond to the Haskell data types and Servant REST APIs

* Persistent: `persistent` is used to automatically generate all database tables, as well as additional types and functions that allow for type-safe database interactions

**Database Re-Initialization**

The `reinitdb.do` script wraps calls that:

* removes the old database (assuming a Sqlite file is stored at the relative path of `../dbsqlite/spaceomnkey.db`)
* calls the database re-initialization script `scripts/Initialize.hs`, which regenerates all database tables and inserts some default values


