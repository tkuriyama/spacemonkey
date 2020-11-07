# Spacemonkey

Experiments in minimizing friction from data boundaries across client, server, and presistence -- while maintainig strict type safety.


- `spacemonkey`: Haskell server and persistance
- `client`: Elm front-end client

At a high-level, the idea is to write Haskell ADTs as needed, then find strategies to minimize the effort / error-prone boilerplate / loss of strict type safety across the front end and persistence layers. 

[Lamdera](https://discourse.elm-lang.org/t/announcing-lamdera-open-alpha/5669) is actually an Elm solution that achieves the same goal from a front-end-centric perspective (abstracting the server and persistence layers away entirely). But it is not currently open-source and remains in [alpha](https://dashboard.lamdera.app/). 


## Build

The build commands for `stack install`, `elm make`, etc are wrapped with [redo](https://redo.readthedocs.io/en/latest/). Running `redo all` in the project root (or the `clietn ` and `spacemonkey` subdirectory roots) triggers rebuilds, but `redo` is not required. The contents of all the `all.do` files can be run as individual shell commands.


## Run

Whichever server that `spacemonkey/server/Main.hs` specifies will be built to the binary `spacemonkey-server` (in Stack's default build location -- on macOS, possibly something like `/Users/$USER/.local/bin/`).

To start the server, run in the command line: `spacemonkey-server`

THe server build also specifies the base URL (e.g. `localhost:8080`) and API endpoints, which are propagated into the generated Elm code.

To start the Elm client, open `client/main.html` in the browser.
