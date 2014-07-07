-- Copyright (C) 2014 Swift Navigation Inc.
-- Contact: Fergus Noble <fergus@swift-nav.com>
--
-- This source is subject to the license found in the file 'LICENSE' which must
-- be be distributed together with this source. All other rights reserved.
--
-- THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
-- EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.

{-# LANGUAGE OverloadedStrings #-}

module Network.Ntrip.Caster.NtripClient (
  runClient
) where

import Control.Concurrent
import Text.Printf
import qualified Data.ByteString.Char8 as B
import Control.Concurrent.STM
import qualified Data.HashMap.Strict as H
import qualified System.Log.Logger as L

import Network.Ntrip.Caster.Types
import Network.Ntrip.Caster.Sourcetable

runClient :: CasterState -> Connection -> B.ByteString -> IO ()
runClient s c mp_name = do
  L.infoM "caster.client" $ printf
    "%s:%s : Client requested mountpoint '%s'"
    (connHost c) (show $ connPort c) (B.unpack mp_name)
  mps <- atomically . readTVar $ mountpoints s
  case H.lookup mp_name mps of
    Nothing  -> do
      -- Mountpoint was now found, send the sourcetable.
      L.warningM "caster.client" $ printf
        "%s:%s : Requested mountpoint '%s' doesn't exist, sending sourcetable"
        (connHost c) (show $ connPort c) (B.unpack mp_name)
      runSourcetable s c
    Just mp -> do
      -- Mountpoint exists
      -- Make our own copy of the Channel
      ch <- dupChan $ channel mp
      let h = handle c
      -- Tell the client that the request was OK
      B.hPutStrLn h "ICY 200 OK"
      -- Start reading data from the mountpoint channel and sending it to the
      -- client.
      loop h ch
  where
    loop h ch = do
      m <- readChan ch
      case m of
        -- Got some data, send it down the socket and wait for more.
        GnssData d -> do
          B.hPut h d
          loop h ch
        -- The mountpoint was removed, close the connection
        MountpointRemoved -> return ()

