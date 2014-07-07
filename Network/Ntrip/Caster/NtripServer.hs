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

module Network.Ntrip.Caster.NtripServer (
  runServer
) where

import Control.Concurrent
import Control.Monad
import Text.Printf
import qualified Data.ByteString.Char8 as B
import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.HashMap.Strict as H
import qualified System.Log.Logger as L

import Network.Ntrip.Caster.Types

createMountpoint :: CasterState -> B.ByteString -> IO (Maybe Mountpoint)
createMountpoint s mp_name = do
  -- TODO: Use some kind of bounded Chan?
  ch <- newChan
  atomically $ do
    mps <- readTVar $ mountpoints s
    case H.lookup mp_name mps of
      Just _  -> return Nothing
      Nothing -> do
        let mp = Mountpoint { channel = ch }
            mps' = H.insert mp_name mp mps
        writeTVar (mountpoints s) mps'
        return $ Just mp

removeMountpoint :: CasterState -> B.ByteString -> IO ()
removeMountpoint s mp_name = atomically $ do
  mps <- readTVar $ mountpoints s
  writeTVar (mountpoints s) $ H.delete mp_name mps

runServer :: CasterState -> Connection -> B.ByteString -> IO ()
runServer s c mp_name = do
  let h = handle c
  E.bracket
    -- Create a new mountpoint and corresponding channel.
    (createMountpoint s mp_name)
    -- Remove mountpoint and notify clients if the server disconnects or an
    -- error occurs.
    (\mmp -> case mmp of
      Just mp -> do
        removeMountpoint s mp_name
        writeChan (channel mp) MountpointRemoved
      Nothing -> return ()
    )
    -- Respond to server and start the main loop.
    $ \mmp -> case mmp of
      Nothing -> do
        -- Mountpoint already exists, send error and close connection.
        B.hPutStrLn h "ERROR - Mount Point Taken"
        L.errorM "caster.server" $ printf
          "%s:%s : Server requested mountpoint '%s' that was already taken"
          (connHost c) (show $ connPort c) (B.unpack mp_name)
      Just mp -> do
        -- Mountpoint available.
        L.infoM "caster.server" $ printf
          "%s:%s : Server connected to mountpoint '%s'"
          (connHost c) (show $ connPort c) (B.unpack mp_name)

        B.hPutStrLn h "ICY 200 OK"
        loop h mp
  where
    loop h mp = do
      -- TODO: Make chunk size configurable
      d <- B.hGetSome h 1024
      unless (d == "") $ do
        writeChan (channel mp) (GnssData d)
        loop h mp

