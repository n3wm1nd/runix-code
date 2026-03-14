{-# LANGUAGE OverloadedStrings #-}

-- | Server-Sent Events (SSE) decoding.
--
-- Re-exports from the @sse-parser@ library.
module Runix.SSE
  ( SSEEvent(..)
  , SSEParseResult(..)
  , parseSSEChunks
  , parseSSEComplete
  , parseEvent
  , splitOnEventTerminator
  ) where

import Network.SSE
