# Sandpit

Small example apps used for leksah demos and screencasts. They are kept out of
the top-level `cabal.project` so leksah's own dev loop is undisturbed; this
directory's `cabal.project` `import:`s the top-level one so the examples build
against leksah's exact jsaddle / reflex / reflex-dom-core plan.

## breakout

A Breakout game copied from the jsaddle-terminal demo. The whole game runs in
Haskell (reflex); it renders through jsaddle. Opened inside leksah it tunnels
over the terminal (jsaddle-terminal) and renders in an IDE iframe; run
standalone it falls back to jsaddle-warp and prints a clickable URL.

Controls: ← → (or A/D) to move, Space (or click) to launch / restart.

### Open in leksah

Open the workspace `sandpit/breakout.lkshw` (File → Open Workspace, or
`leksah-cmd project open sandpit/cabal.project`), then build and run the
`breakout` executable.

### Build / run standalone

    cd sandpit
    cabal build breakout
    cabal run  breakout      # prints a jsaddle-warp URL to open in a browser
