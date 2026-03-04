{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}


module Runix.HTTP.RequestLogger where

import Prelude hiding (writeFile)
import Polysemy
import Polysemy.Fail
import Polysemy.State
import qualified Data.ByteString.Lazy as BSL
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Data.Aeson (ToJSON(..), object, (.=), encode)
import qualified Data.Text.Encoding as T
import qualified Data.Map.Strict as Map

import Runix.HTTP (HTTP(..), HTTPRequest, HTTPResponse, HTTPStreaming, HTTPStreamResult(..))
import qualified Runix.HTTP as HTTP
import Runix.Time (Time, getCurrentTime)
import Runix.FileSystem (FileSystemWrite, writeFile, fileSystemLocal)
import qualified Runix.FileSystem.System
import Runix.Streaming (Streaming(..), StreamId(..))
import Config (RequestLogFS(..))

-- | JSON encoding for HTTPRequest
instance ToJSON HTTPRequest where
  toJSON (HTTP.HTTPRequest method uri headers body) = object
    [ "method" .= method
    , "uri" .= uri
    , "headers" .= headers
    , "body" .= fmap (T.decodeUtf8 . BSL.toStrict) body
    ]

-- | JSON encoding for HTTPResponse
instance ToJSON HTTPResponse where
  toJSON (HTTP.HTTPResponse code headers body) = object
    [ "status_code" .= code
    , "headers" .= headers
    , "body" .= (T.decodeUtf8 . BSL.toStrict) body
    ]

-- | JSON encoding for logged request/response pair
data LoggedRequest = LoggedRequest
  { logTimestamp :: UTCTime
  , logRequest :: HTTPRequest
  , logResponse :: Either String HTTPResponse
  }

instance ToJSON LoggedRequest where
  toJSON logged = object
    [ "timestamp" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (logTimestamp logged)
    , "request" .= logRequest logged
    , "response" .= case logResponse logged of
        Right resp -> toJSON resp
        Left err -> object ["error" .= err]
    ]

-- | Intercept HTTP requests and log them to filesystem
-- The filesystem @fs should be set up to point to the logging directory
logHTTPRequests :: forall fs r a.
                   Members [HTTP, Time, FileSystemWrite fs, Fail] r
                => Sem r a
                -> Sem r a
logHTTPRequests = intercept $ \case
    HttpRequest request -> do
      timestamp <- getCurrentTime
      response <- send (HttpRequest request)

      -- Create log entry
      let logged = LoggedRequest
            { logTimestamp = timestamp
            , logRequest = request
            , logResponse = response
            }

      -- Write to log file (relative to the fs root)
      let filename = formatTime defaultTimeLocale "http-%Y%m%d-%H%M%S%Q.json" timestamp

      -- Write log entry
      writeFile @fs filename (BSL.toStrict $ encode logged)

      -- Return original response
      return response

-- | State for tracking streaming requests by StreamId
type StreamRequestMap = Map.Map StreamId (UTCTime, HTTPRequest)

-- | Intercept streaming HTTP requests and log them to filesystem
-- Uses State to track request config across StartStream/CloseStream calls
logHTTPStreamingRequests :: forall fs r a.
                            Members '[HTTPStreaming, Time, FileSystemWrite fs, Fail] r
                         => Sem r a
                         -> Sem r a
logHTTPStreamingRequests action =
  evalState (Map.empty :: StreamRequestMap) $ intercept @HTTPStreaming (\case
    StartStream config -> do
      timestamp <- getCurrentTime
      result <- send @HTTPStreaming $ StartStream config

      -- Store request config for this stream
      case result of
        Right sid -> modify @StreamRequestMap $ Map.insert sid (timestamp, config)
        Left _ -> return ()

      return result

    FetchItem sid -> send @HTTPStreaming $ FetchItem sid

    CloseStream sid -> do
      result <- send @HTTPStreaming $ CloseStream sid

      -- Retrieve the request config for this stream
      requestMap <- get @StreamRequestMap
      case Map.lookup sid requestMap of
        Just (timestamp, config) -> do
          -- Write combined request/response log
          let filename = formatTime defaultTimeLocale "http-streaming-%Y%m%d-%H%M%S%Q.json" timestamp
          let logEntry = object
                [ "timestamp" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" timestamp
                , "request" .= config
                , "response" .= object
                    [ "status_code" .= streamStatusCode result
                    , "headers" .= streamHeaders result
                    , "body" .= (T.decodeUtf8 . BSL.toStrict . streamBody) result
                    ]
                ]
          writeFile @fs filename (BSL.toStrict $ encode logEntry)

          -- Clean up the map
          modify @StreamRequestMap $ Map.delete sid
        Nothing -> return ()

      return result
  ) $ raise action


-- | Composite function: set up logging filesystem and intercept HTTP requests
-- This composes: create log dir . fileSystemLocal . logHTTPRequests . logHTTPStreamingRequests
withHTTPRequestLogging :: forall r a.
                          Members '[HTTPStreaming, HTTP, Time, Runix.FileSystem.System.FileSystemRead, Runix.FileSystem.System.FileSystemWrite, Fail] r
                       => FilePath  -- ^ Current working directory
                       -> Sem r a
                       -> Sem r a
withHTTPRequestLogging cwd action = do
  let logDir = cwd ++ "/.runix/logs"
  -- Create log directory using System filesystem
  _ <- Runix.FileSystem.System.createDirectory True logDir
  -- Set up RequestLogFS and intercept HTTP
  -- Need to raise3 to add the three filesystem effects
  fileSystemLocal (RequestLogFS logDir)
    $ logHTTPRequests @RequestLogFS
    $ logHTTPStreamingRequests @RequestLogFS
    $ raise $ raise $ raise action
