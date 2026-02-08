{-# LANGUAGE OverloadedStrings #-}

-- | Server-Sent Events (SSE) parser
--
-- Implements the SSE protocol as defined in:
-- https://html.spec.whatwg.org/multipage/server-sent-events.html#server-sent-events
--
-- SSE events are separated by blank lines. Each event consists of fields:
-- - event: the event type (defaults to "message")
-- - data: the event data (can appear multiple times, concatenated with newlines)
-- - id: the event ID
-- - retry: reconnection time in milliseconds
-- - Lines starting with : are comments and ignored
module Runix.SSE
  ( -- * Types
    SSEEvent(..)
  , defaultSSEEvent

    -- * Conduit Interface
  , sseParser
  , splitOnBlankLines
  , parseSSEEvents

    -- * Simple Interface
  , parseSSEComplete
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Conduit
import qualified Data.Conduit.Combinators as CC
import Control.Monad (when)

-- | An SSE event with parsed fields
data SSEEvent = SSEEvent
  { eventType :: Text      -- ^ Event type (defaults to "message")
  , eventData :: Text      -- ^ Event data (multiple data: fields concatenated with newlines)
  , eventId :: Maybe Text  -- ^ Event ID (if specified)
  , eventRetry :: Maybe Int -- ^ Retry interval in milliseconds (if specified)
  } deriving (Show, Eq)

-- | Default empty event (used as accumulator)
defaultSSEEvent :: SSEEvent
defaultSSEEvent = SSEEvent
  { eventType = "message"
  , eventData = ""
  , eventId = Nothing
  , eventRetry = Nothing
  }

-- | Complete SSE parser pipeline
sseParser :: Monad m => ConduitT ByteString SSEEvent m ()
sseParser = splitOnBlankLines .| parseSSEEvents

-- | Split ByteString stream into chunks separated by blank lines
-- Handles buffering of partial lines across chunks
splitOnBlankLines :: Monad m => ConduitT ByteString ByteString m ()
splitOnBlankLines = go BS.empty BS.empty
  where
    newline = 10 :: Word8  -- '\n'

    go :: Monad m => ByteString -> ByteString -> ConduitT ByteString ByteString m ()
    go buffer accumulator = do
      mChunk <- await
      case mChunk of
        Nothing ->
          -- End of stream - discard any partial data (no blank line = no event)
          return ()

        Just chunk -> do
          let input = buffer <> chunk
              lines' = BS.split newline input
          processLines accumulator lines'

    processLines :: Monad m => ByteString -> [ByteString] -> ConduitT ByteString ByteString m ()
    processLines accumulator [] = go BS.empty accumulator
    processLines accumulator [lastLine] =
      -- Last line might be incomplete, keep as buffer
      go lastLine accumulator
    processLines accumulator (line:rest) = do
      if BS.null line
        then do
          -- Blank line - emit accumulated event if non-empty
          when (not $ BS.null accumulator) $
            yield accumulator
          processLines BS.empty rest
        else do
          -- Add line to accumulator (with newline)
          let newAccum = if BS.null accumulator
                         then line
                         else accumulator <> "\n" <> line
          processLines newAccum rest

-- | Parse SSE event chunks into structured SSEEvent values
parseSSEEvents :: Monad m => ConduitT ByteString SSEEvent m ()
parseSSEEvents = awaitForever $ \chunk -> do
  case parseSSEEvent chunk of
    Left _err -> return ()  -- Skip malformed events
    Right event -> yield event

-- | Parse a single SSE event chunk
parseSSEEvent :: ByteString -> Either Text SSEEvent
parseSSEEvent chunk =
  let text = TE.decodeUtf8 chunk
      lines' = T.lines text
      event = foldl processLine defaultSSEEvent lines'
  in Right event
  where
    processLine :: SSEEvent -> Text -> SSEEvent
    processLine event line
      -- Comment line - ignore
      | T.isPrefixOf ":" line = event
      -- Field with colon
      | T.elem ':' line =
          let (fieldName, rest) = T.breakOn ":" line
              -- Drop the colon and optional leading space
              fieldValue = T.drop 1 rest
              fieldValue' = if T.isPrefixOf " " fieldValue
                           then T.drop 1 fieldValue
                           else fieldValue
          in processField event fieldName fieldValue'
      -- Field without value (treat as empty value)
      | otherwise = processField event line ""

    processField :: SSEEvent -> Text -> Text -> SSEEvent
    processField event "event" value = event { eventType = value }
    processField event "data" value =
      let currentData = eventData event
          newData = if T.null currentData
                    then value
                    else currentData <> "\n" <> value
      in event { eventData = newData }
    processField event "id" value = event { eventId = Just value }
    processField event "retry" value =
      case reads (T.unpack value) of
        [(n, "")] -> event { eventRetry = Just n }
        _ -> event  -- Invalid retry value, ignore
    processField event _ _ = event  -- Unknown field, ignore

-- | Parse a complete SSE response (non-streaming)
-- Runs the conduit pipeline to completion and returns all events
parseSSEComplete :: ByteString -> [SSEEvent]
parseSSEComplete body = runConduitPure $
  CC.sourceLazy (BSL.fromStrict body) .| sseParser .| CC.sinkList
