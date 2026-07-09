-- | A small Language Server Protocol client built directly on the protocol
-- types from @lsp-types@ (and nothing else — in particular not the @lsp@
-- server framework).
--
-- Typical use from an editor:
--
-- @
-- client <- 'start' \"haskell-language-server\" [\"--lsp\"] (Just projectRoot) (Just env)
--             'defaultClientConfig'
--               { 'onNotification' = \\method params -> case method of
--                   \"textDocument/publishDiagnostics\" -> handleDiagnostics params
--                   _ -> pure ()
--               }
-- _ <- 'request' client SMethod_Initialize initParams $ \\res -> case res of
--        Right initResult -> 'notify' client SMethod_Initialized (Just ...)
--        Left err         -> reportError err
-- @
--
-- The session is concurrent and callback-driven: 'request' returns
-- immediately and its callback fires when the response arrives, while
-- server-initiated notifications and requests are delivered through the
-- 'ClientConfig' handlers at any time.
module Language.LSP.Client
    ( -- * Session
      Client
    , ClientConfig(..)
    , defaultClientConfig
    , start
    , stop
    , request
    , notify
      -- * Transport (for callers that manage the process themselves)
    , ServerHandles(..)
    ) where

import Language.LSP.Client.Session
import Language.LSP.Client.Transport (ServerHandles(..))
