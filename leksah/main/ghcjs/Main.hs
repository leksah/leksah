module Main (main) where

import IDE.Web.Main (browserMain)

-- The GHC-JavaScript-backend front end: the whole leksah web UI compiled to
-- JavaScript and loaded by the hosting page (docs/website).  There is no
-- native window and no warp server here — `browserMain` runs the shared
-- `jsMain` directly against the page's own DOM via jsaddle-warp's
-- in-browser `run` (a base-only shim under this backend).  Native
-- subsystems (build tools, PTYs, sockets, LSP servers, the real
-- filesystem) don't exist in a browser; the web-demo build stubs them.
main :: IO ()
main = browserMain
