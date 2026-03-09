{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Application-specific wrapper for HTTP file logging
--
-- This module provides the RequestLogFS phantom type and a convenience
-- function that sets up the logging directory and wires up the interceptors
-- from Runix.Tracing.FileLog.
module Runix.HTTP.RequestLogger
  ( withHTTPFileLogging
  ) where

import Polysemy
import Polysemy.Fail

import Config (RequestLogFS(..))
import Runix.HTTP (HTTP, HTTPStreaming)
import Runix.Time (Time)
import Runix.FileSystem (fileSystemLocal)
import qualified Runix.FileSystem.System
import Runix.Tracing.FileLog (logHTTPRequests, logHTTPStreamingRequests)

-- | Composite function: set up logging filesystem and intercept HTTP requests
-- This composes: create log dir . fileSystemLocal . logHTTPRequests . logHTTPStreamingRequests
--
-- Logs are written to @projectDir/.runix/logs/@
withHTTPFileLogging :: forall r a.
                       Members '[HTTPStreaming, HTTP, Time, Runix.FileSystem.System.FileSystemRead, Runix.FileSystem.System.FileSystemWrite, Fail] r
                    => FilePath  -- ^ Current working directory
                    -> Sem r a
                    -> Sem r a
withHTTPFileLogging cwd action = do
  let logDir = cwd ++ "/.runix/logs"
  -- Create log directory using System filesystem
  _ <- Runix.FileSystem.System.createDirectory True logDir
  -- Set up RequestLogFS and intercept HTTP
  -- Need to raise3 to add the three filesystem effects
  fileSystemLocal (RequestLogFS logDir)
    $ logHTTPRequests @RequestLogFS
    $ logHTTPStreamingRequests @RequestLogFS
    $ raise $ raise $ raise action
